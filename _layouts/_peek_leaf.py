# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
for nid in ['aIwhuIM42UXZ','KENrlyFaUbZN','3GTPwnM0LXzq']:
    row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
    print('='*100)
    print('%s | %s | len=%d' % (nid, row[0], len(c)))
    print(c[:3000])
conn.close()
