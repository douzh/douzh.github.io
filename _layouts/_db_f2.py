# -*- coding: utf-8 -*-
import sqlite3, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# find the tray-icon bilingual note
rows=cur.execute("""
SELECT n.noteId, n.title, n.type, n.mime, n.blobId
FROM notes n
WHERE n.isDeleted=0 AND (n.title LIKE '%Tray%' OR n.title LIKE '%tray%' OR n.title LIKE '%Quick Start%' OR n.title LIKE '%快速开始%')
""").fetchall()
print('tray/quickstart notes:')
for r in rows:
    print('  ', r)

# inspect tray bilingual content
for r in rows:
    nid,title,typ,mime,bid=r
    blob=cur.execute("SELECT content FROM blobs WHERE blobId=?", (bid,)).fetchone()
    c=blob[0] if blob and blob[0] else b''
    print('\n=== note:', nid, title, 'type:', typ, 'blob len:', len(c))
    if c:
        txt=c.decode('utf-8','replace')
        print(txt[:1800])
