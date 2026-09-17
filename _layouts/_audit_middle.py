# -*- coding: utf-8 -*-
import sqlite3, json, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

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

books = []
bilingual = []
plainmd = []
empty = []
for nid, title, typ, blobId in nodes:
    content = get_blob(blobId) or ''
    if typ == 'book':
        books.append((nid, title, typ, len(content)))
    # 双语：内容同时含较多中文和英文单词
    cn = len(re.findall(r'[\u4e00-\u9fff]', content))
    en = len(re.findall(r'[A-Za-z]{3,}', content))
    if typ == 'text' and cn > 50 and en > 50 and cn < 5000 and en < 5000:
        bilingual.append((nid, title, typ, len(content), cn, en))
    # 纯文本 markdown（text 类型但内容不是 HTML）
    if typ == 'text' and content and not re.search(r'<p|<div|<h[1-6]|<ul|<table', content) and cn > 20:
        plainmd.append((nid, title, typ, len(content)))
    if typ in ('text','code') and not content.strip():
        empty.append((nid, title, typ))

print('\n=== book 类型笔记 (%d) ===' % len(books))
for r in books:
    print('  %s | %s | %s | len=%d' % r)

print('\n=== 疑似双语混排 text 笔记 (%d) ===' % len(bilingual))
for r in bilingual:
    print('  %s | %s | len=%d cn=%d en=%d' % r)

print('\n=== text 但内容是纯文本/markdown (%d) ===' % len(plainmd))
for r in plainmd:
    print('  %s | %s | len=%d' % r)

print('\n=== 空内容 text/code (%d) ===' % len(empty))
for r in empty:
    print('  %s | %s' % (r[0], r[1]))
conn.close()
