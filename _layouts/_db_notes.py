# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
# notes schema
print('notes schema:', [r[1] for r in cur.execute("PRAGMA table_info(notes)").fetchall()])
# find _help notes
rows=cur.execute("SELECT noteId, title, type, mime, blobId, content FROM notes WHERE noteId LIKE '_help%' LIMIT 5").fetchall()
print()
print('_help notes sample:')
for r in rows:
    print('  ', r)
print()
# count notes by type for _help
rows=cur.execute("SELECT type, COUNT(*) FROM notes WHERE noteId LIKE '_help%' GROUP BY type").fetchall()
print('_help types:', rows)
# blobs count
print('blobs total:', cur.execute("SELECT COUNT(*) FROM blobs").fetchone()[0])
# blobs sample
rows=cur.execute("SELECT blobId, length(content) FROM blobs LIMIT 10").fetchall()
print('blobs sample:', rows)
