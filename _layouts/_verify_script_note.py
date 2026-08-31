# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
rows=cur.execute("SELECT noteId, title, type, mime FROM notes WHERE noteId='vhtl2SYLJG2M'").fetchall()
for r in rows: print(r)
# check its parent
rows=cur.execute("SELECT branchId, parentNoteId FROM branches WHERE noteId='vhtl2SYLJG2M' AND isDeleted=0").fetchall()
for r in rows: print('branch:', r)
# attributes on script note
rows=cur.execute("SELECT name,value FROM attributes WHERE noteId='vhtl2SYLJG2M' AND isDeleted=0").fetchall()
print('attrs:', rows)
