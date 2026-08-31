# -*- coding: utf-8 -*-
import sqlite3, json, struct, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
# find help notes about scripting, runOnNoteLoad, resource provider
rows=cur.execute("SELECT noteId, title FROM notes WHERE isDeleted=0 AND noteId LIKE '_help%' AND type='doc' AND (title LIKE '%cript%' OR title LIKE '%runOn%' OR title LIKE '%resource%' OR title LIKE '%Provider%' OR title LIKE '%widget%' OR title LIKE '%Script%')").fetchall()
for r in rows:
    print(r)
print()
# docName for scripting notes
rows=cur.execute("""
SELECT a.noteId, n.title, a.value FROM attributes a JOIN notes n ON n.noteId=a.noteId
WHERE a.isDeleted=0 AND a.type='label' AND a.name='docName' AND a.noteId LIKE '_help%'
AND (a.value LIKE '%cript%' OR a.value LIKE '%resource%' OR a.value LIKE '%Provider%')
""").fetchall()
for r in rows:
    print(r)
