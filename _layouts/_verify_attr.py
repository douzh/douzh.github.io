# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
rows=cur.execute("SELECT noteId, name, value FROM attributes WHERE noteId='3GTPwnM0LXzq' AND isDeleted=0").fetchall()
for r in rows:
    print(r)
