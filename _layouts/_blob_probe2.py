# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
for nid,label in [('Q0A1Gg3pgIOP','笔记类型'),('B5oluBvu2gBY','文本'),('gBJUFdOLz9GA','脚本')]:
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (n[0],)).fetchone()
    c=b[0] if b else ''
    print(f'=== {label} === len={len(c)}')
    print(repr(c[:400]))
    print()
