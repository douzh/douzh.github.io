# -*- coding: utf-8 -*-
import sqlite3, json, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

# 从 tree 表获取子树
root = 'yTRmjAPEV7Wm'
def subtree(note_id):
    rows = cur.execute("""
      SELECT b.noteId, n.title, n.type, n.blobId
      FROM branches b JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId = ? ORDER BY b.notePosition, b.noteId
    """, (note_id,)).fetchall()
    out = []
    for r in rows:
        out.append(r)
        out += subtree(r[0])
    return out

nodes = subtree(root)
print('总节点数:', len(nodes))
# 找 导入/导出 相关
for nid, title, typ, blobId in nodes:
    if title and ('导入' in title or '导出' in title or 'Import' in title or 'Export' in title):
        content = get_blob(blobId) or ''
        print('-'*90)
        print('%s | %s | %s | len=%d' % (nid, title, typ, len(content)))
        print(content[:1500])
conn.close()
