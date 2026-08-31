# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
tabs=[r[0] for r in cur.execute("SELECT name FROM sqlite_master WHERE type='table'").fetchall()]
print('tables:', tabs)
print()
# attachments
print('attachments total:', cur.execute("SELECT COUNT(*) FROM attachments").fetchone()[0])
# notes with chinese/paren titles
rows=cur.execute("""
SELECT n.noteId, n.title, n.type, n.mime, n.blobId
FROM notes n
WHERE n.isDeleted=0 AND (n.title LIKE '%(%' OR n.title LIKE '%（%' OR n.title LIKE '%指南%' OR n.title LIKE '%用户%' OR n.title LIKE '%入门%' OR n.title LIKE '%帮助%')
LIMIT 40
""").fetchall()
print('\nparen/chinese notes:', len(rows))
for r in rows:
    print('  ', r)
