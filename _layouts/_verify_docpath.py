# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
rows=cur.execute("SELECT noteId, value FROM attributes WHERE name='docPath' AND isDeleted=0").fetchall()
print('docPath attributes total:', len(rows))
for r in rows[:8]:
    print(' ', r)
