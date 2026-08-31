# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
# schema
print('attachments schema:', [r[1] for r in cur.execute("PRAGMA table_info(attachments)").fetchall()])
print('blobs schema:', [r[1] for r in cur.execute("PRAGMA table_info(blobs)").fetchall()])
print()
# count attachments
print('attachments total:', cur.execute("SELECT COUNT(*) FROM attachments").fetchone()[0])
# attachments for _help notes
rows=cur.execute("SELECT noteId, attachmentId, title, role, mime FROM attachments WHERE noteId LIKE '_help%' LIMIT 20").fetchall()
print('attachments on _help notes:', len(rows))
for r in rows:
    print('  ', r)
