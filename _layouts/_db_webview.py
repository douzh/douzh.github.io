# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
# webView nodes in _help
rows=cur.execute("SELECT noteId, title, blobId FROM notes WHERE noteId LIKE '_help%' AND type='webView'").fetchall()
print('webView help nodes:')
for r in rows:
    print('  ', r)
# check blobs content for these
for r in rows:
    blob=cur.execute("SELECT content, textRepresentation FROM blobs WHERE blobId=?", (r[2],)).fetchone()
    print('  blob for', r[1], ':', (blob[0] if blob else None))
# help doc nodes content - check a doc note content via blobs
blob=cur.execute("SELECT content FROM blobs WHERE blobId='z4PhNX7vuL3xVChQ1m2A'").fetchone()
print('empty blob content:', repr(blob))
# find where help doc content actually lives - check revisions
print()
print('revisions schema:', [x[1] for x in cur.execute("PRAGMA table_info(revisions)").fetchall()])
rows=cur.execute("SELECT noteId, title, blobId FROM revisions WHERE noteId='_help_7iwde4lsKc6O'").fetchall()
print('revisions for tray note:', rows)
for r in rows:
    blob=cur.execute("SELECT length(content) FROM blobs WHERE blobId=?", (r[2],)).fetchone()
    print('  rev blob len:', blob)
