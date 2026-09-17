# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
nid = 'aIwhuIM42UXZ'
row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
print('%s | %s' % (nid, row[0]))
print(c)
conn.close()
