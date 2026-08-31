# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
rows=cur.execute("SELECT attachmentId, ownerId, role, mime, title FROM attachments LIMIT 30").fetchall()
print('all attachments:')
for r in rows:
    print('  ', r)
