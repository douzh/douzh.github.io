# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

for nid in ['6at3wkEnLIfu','3GTPwnM0LXzq']:
    blob=cur.execute("SELECT content, textRepresentation FROM blobs WHERE blobId=(SELECT blobId FROM notes WHERE noteId=?)", (nid,)).fetchone()
    print('=== note:', nid, '===')
    if blob:
        c=blob[0]
        print('content type:', type(c), 'len:', len(c))
        if isinstance(c,str):
            print(c[:1800])
        else:
            print(c.decode('utf-8','replace')[:1800])
    print()
