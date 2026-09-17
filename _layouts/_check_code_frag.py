# -*- coding: utf-8 -*-
import sqlite3, json
DB = r'F:\trilium\trilium-data\document.db'
nid = 'JmvAMQu8gb4o'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
print('title:', row[0])
content = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
print('len:', len(content))
print(content)
conn.close()
