# -*- coding: utf-8 -*-
import sqlite3, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

rows=cur.execute("""
SELECT n.noteId, n.title, n.blobId
FROM notes n
WHERE n.isDeleted=0 AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%' AND n.noteId NOT LIKE '_lb%'
AND n.title LIKE '%(%' AND n.type='text'
""").fetchall()
print('bilingual text notes:', len(rows))
