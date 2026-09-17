# -*- coding: utf-8 -*-
"""抽查 p 段落中英文残留较多者，区分代码块内英文与正文英文"""
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
for nid in ['sPaAXjVcvZZl','o5byGpiYwkAf','mWpKWRZZLu3R']:
    row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
    print('='*100)
    print('### %s %s' % (nid, row[0]))
    # 只看 <p> 段落（非代码块）
    for m in re.finditer(r'<p[^>]*>(.*?)</p>', c, re.S):
        p = m.group(1)
        txt = re.sub(r'<[^>]+>', ' ', p)
        fc = re.search(r'[\u4e00-\u9fff]', txt)
        before = txt[:fc.start()] if fc else txt
        en_before = len(re.findall(r'[A-Za-z]{3,}', before))
        if en_before > 10:
            print('  [p 英文残留 %d] %s' % (en_before, txt[:150]))
conn.close()
