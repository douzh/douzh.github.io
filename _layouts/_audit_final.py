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

# 1) book 类型
books = [(n[0],n[1],len(get_blob(n[3]) or '')) for n in nodes if n[2]=='book']
print('\n=== book 类型 (%d) ===' % len(books))
for r in books:
    print('  %s | %s | len=%d' % r)

# 2) text/code 但内容非HTML且含中文（一坨文字）
plain = [(n[0],n[1],len(get_blob(n[3]) or '')) for n in nodes
         if n[2] in ('text','code') and (get_blob(n[3]) or '') and
         not re.search(r'<p|<div|<h[1-6]|<ul|<table', get_blob(n[3]) or '') and
         re.search(r'[\u4e00-\u9fff]', get_blob(n[3]) or '')]
print('\n=== text/code 非HTML含中文 (%d) ===' % len(plain))
for r in plain:
    print('  %s | %s | len=%d' % r)

# 3) text 内容为双语混排（英文字母多且与中文混排，段落级交替）——识别"英文段落+中文段落"模式
dual = []
for n in nodes:
    if n[2] != 'text':
        continue
    c = get_blob(n[3]) or ''
    if not c or not re.search(r'<p', c):
        continue
    # 提取段落纯文本
    paras = re.findall(r'<p>(.*?)</p>', c, re.S)
    txt = ' '.join(re.sub(r'<[^>]+>','',p) for p in paras)
    cn = len(re.findall(r'[\u4e00-\u9fff]', txt))
    en = len(re.findall(r'[A-Za-z]{3,}', txt))
    if cn > 200 and en > 200:
        dual.append((n[0], n[1], len(c), cn, en))
print('\n=== 疑似双语混排 text (cn>200 & en>200) (%d) ===' % len(dual))
for r in dual:
    print('  %s | %s | len=%d cn=%d en=%d' % r)
conn.close()
