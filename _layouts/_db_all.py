# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

rows=cur.execute("""
SELECT n.noteId, n.title, n.type, n.mime, n.blobId
FROM notes n
WHERE n.isDeleted=0 AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%' AND n.noteId NOT LIKE '_lb%'
ORDER BY n.dateCreated DESC
""").fetchall()
print('all user notes:', len(rows))
for r in rows[:80]:
    print('  ', r)
