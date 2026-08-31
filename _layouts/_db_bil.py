# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# find user-created bilingual notes under 用户指南
# first find branches/notes named like bilingual ones
rows=cur.execute("""
SELECT n.noteId, n.title, n.type, n.blobId
FROM notes n
WHERE n.title LIKE '%Tray icon%' OR n.title LIKE '%快速入门%' OR n.title LIKE '%Quick Start%' OR n.title LIKE '%用户指南%'
""").fetchall()
print('candidate bilingual notes:')
for r in rows:
    print('  ', r)

# check the tray-icon bilingual note content
tray=[r for r in rows if 'Tray icon' in (r[1] or '')]
print()
print('tray note rows:', tray)
for r in tray:
    blob=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[3],)).fetchone()
    if blob and blob[0]:
        c=blob[0]
        print('--- note', r[0], r[1], 'len', len(c))
        print(c[:1500])
