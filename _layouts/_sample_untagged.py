# -*- coding: utf-8 -*-
import sqlite3, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

samples=['6at3wkEnLIfu','NRzmLnGiDb8U','eT4hTrNrfbR0','Td5XP4paGGvm','t9tDQ3yYHSIQ','v4jDFrdyA3XU']
for nid in samples:
    r=cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    if not r: 
        print(nid, 'NOT FOUND'); continue
    title, blob=r
    # read blob content
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob,)).fetchone()
    content=b[0] if b else ''
    if isinstance(content, bytes):
        content=content.decode('utf-8','replace')
    # 取纯文本开头 400 字
    text=re.sub(r'<[^>]+>',' ', content)
    text=re.sub(r'\s+',' ', text).strip()
    print(f'=== {nid} | {title} | blob={blob} | len={len(content)}')
    print('   ', text[:350])
    print()
