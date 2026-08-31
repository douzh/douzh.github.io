# -*- coding: utf-8 -*-
import sqlite3, re
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# notes with chinese title or parens
rows=cur.execute("""
SELECT n.noteId, n.title, n.type, n.mime, n.blobId, n.dateCreated
FROM notes n
WHERE n.isDeleted=0 AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%' AND n.noteId NOT LIKE '_lb%'
AND (n.title LIKE '%(%' OR n.title LIKE '%（%' OR n.title LIKE '%用户%' OR n.title LIKE '%指南%' OR n.title LIKE '%帮助%' OR n.title LIKE '%入门%' OR n.title LIKE '%安装%' OR n.title LIKE '%欢迎%' OR n.title LIKE '%介绍%')
ORDER BY n.dateCreated
""").fetchall()
print('paren/chinese title notes:', len(rows))
for r in rows:
    print('  ', r)

# Also check text notes whose blob contains '（' and ') ' english chinese pairs - scan recent text notes content for img + chinese
print()
print('=== scan text notes content for bilingual+img ===')
rows=cur.execute("SELECT n.noteId, n.title, n.blobId FROM notes n WHERE n.type='text' AND n.isDeleted=0 AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%'").fetchall()
found=[]
for nid,title,bid in rows:
    if not bid or bid=='z4PhNX7vuL3xVChQ1m2A': continue
    c=cur.execute("SELECT content FROM blobs WHERE blobId=?", (bid,)).fetchone()
    if not c or not c[0]: continue
    txt=c[0]
    if '<img' in txt and re.search(r'[\u4e00-\u9fff]', txt):
        found.append((nid,title,len(txt)))
print('text notes with img AND chinese:', len(found))
for f in found[:60]:
    print('  ', f)
