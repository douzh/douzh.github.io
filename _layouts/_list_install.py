# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

def subtree(note_id, depth=0):
    rows = cur.execute("""
      SELECT b.noteId, n.title, n.type, n.blobId
      FROM branches b JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId = ? ORDER BY b.notePosition, b.noteId
    """, (note_id,)).fetchall()
    out = []
    for r in rows:
        out.append((r[0], r[1], r[2], depth))
        out += subtree(r[0], depth+1)
    return out

for root_id, root_name in [('39unKGhUFGBr','桌面安装'), ('z5TN5NCWzga3','服务器安装')]:
    print('='*100)
    print('## %s (%s)' % (root_name, root_id))
    nodes = subtree(root_id)
    for nid, title, typ, depth in nodes:
        row = cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
        c = get_blob(row[0]) if row else ''
        # 判断是否双语：文本级英文+中文都较多
        txt = re.sub(r'<[^>]+>', ' ', c)
        cn = len(re.findall(r'[\u4e00-\u9fff]', txt))
        en = len(re.findall(r'[A-Za-z]{3,}', txt))
        flag = '双语' if (cn>100 and en>100 and typ=='text') else ''
        print('  '*(depth+1) + '%s | %s | %s | len=%d cn=%d en=%d %s' % (nid, (title or '')[:40], typ, len(c), cn, en, flag))
conn.close()
