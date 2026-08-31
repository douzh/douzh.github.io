# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# all bilingual text notes (title with '(' format) that are user-created (not _help)
rows=cur.execute("""
SELECT n.noteId, n.title, n.blobId
FROM notes n
WHERE n.isDeleted=0 AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%' AND n.noteId NOT LIKE '_lb%'
AND n.title LIKE '%(%' AND n.type='text'
""").fetchall()
print('bilingual text notes:', len(rows))

total_imgs=0
notes_with_img=0
for nid,title,bid in rows:
    if not bid or bid=='z4PhNX7vuL3xVChQ1m2A': continue
    c=cur.execute("SELECT content FROM blobs WHERE blobId=?", (bid,)).fetchone()
    if not c or not c[0]: continue
    txt=c[0]
    imgs=re.findall(r'<img[^>]*src="([^"]+)"', txt)
    imgs=[x for x in imgs if not x.startswith('http') and not x.startswith('api/') and not x.startswith('#root') and not x.startswith('data:')]
    if imgs:
        notes_with_img+=1
        total_imgs+=len(imgs)
        print(f'\n  {nid} | {title} | imgs: {len(imgs)}')
        for im in imgs[:4]:
            print('      src:', im)
print('\nTOTAL notes with images:', notes_with_img, '| total imgs:', total_imgs)
