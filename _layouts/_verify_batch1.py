# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
for nid in ['JmvAMQu8gb4o','2IpMVDLsydKq','GN7LU2N7OhJi']:
    n=cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (n[1],)).fetchone()
    c=b[0] if b else ''
    print(f'=== {n[0]} ({nid}) ===')
    print(repr(c[:300]))
    print()
