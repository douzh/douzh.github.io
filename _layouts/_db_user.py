# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# search notes with titles containing 指南/Guide/Start/intro etc that are NOT _help
rows=cur.execute("""
SELECT n.noteId, n.title, n.type, n.mime, n.blobId
FROM notes n
WHERE (n.title LIKE '%指南%' OR n.title LIKE '%Guide%' OR n.title LIKE '%Start%' OR n.title LIKE '%启动%' OR n.title LIKE '%安装%' OR n.title LIKE '%快速%' OR n.title LIKE '%入门%' OR n.title LIKE '%User%' OR n.title LIKE '%Tray%' OR n.title LIKE '%tray%')
AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%' AND n.isDeleted=0
""").fetchall()
print('user notes candidates:', len(rows))
for r in rows:
    print('  ', r)
