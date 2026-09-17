# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
for nid in ['eaZEEY2i4YT2','3jBts3KWOAAP','v14qii09xWVc']:
    row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
    print('='*100)
    print('### %s %s' % (nid, row[0]))
    # 移除代码块后看正文
    c_no_code = re.sub(r'<pre.*?</pre>', '【CODE】', c, flags=re.S)
    c_no_code = re.sub(r'<code[^>]*>.*?</code>', '【C】', c_no_code, flags=re.S)
    for m in re.finditer(r'<p[^>]*>(.*?)</p>', c_no_code, re.S):
        p = m.group(1)
        txt = re.sub(r'<[^>]+>', ' ', p)
        fc = re.search(r'[\u4e00-\u9fff]', txt)
        before = txt[:fc.start()] if fc else txt
        en_before = len(re.findall(r'[A-Za-z]{3,}', before))
        if en_before > 8:
            print('  [正文英文残留 %d] %s' % (en_before, txt[:120]))
    print('  （以上为移除了代码块后的正文段落检查）')
conn.close()
