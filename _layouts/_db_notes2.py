# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
rows=cur.execute("SELECT noteId, title, type, mime, blobId FROM notes WHERE noteId LIKE '_help%' LIMIT 8").fetchall()
print('_help notes sample:')
for r in rows:
    print('  ', r)
print()
rows=cur.execute("SELECT type, COUNT(*) FROM notes WHERE noteId LIKE '_help%' GROUP BY type").fetchall()
print('_help types:', rows)
print('blobs total:', cur.execute("SELECT COUNT(*) FROM blobs").fetchone()[0])
rows=cur.execute("SELECT blobId, length(content) FROM blobs LIMIT 12").fetchall()
print('blobs sample:', rows)
# are there blobs referenced by _help notes?
rows=cur.execute("SELECT n.noteId, n.title, n.blobId FROM notes n WHERE n.noteId LIKE '_help%' AND n.blobId IS NOT NULL AND n.blobId != '' LIMIT 10").fetchall()
print('_help notes with blobId:', rows)
