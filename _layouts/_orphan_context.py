# -*- coding: utf-8 -*-
import sqlite3, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
def blob_str(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    c=r[0]
    return c if isinstance(c,str) else c.decode('utf-8','replace')

n=cur.execute("SELECT blobId FROM notes WHERE noteId='s5yEiZdU32Oh'",).fetchone()
text=blob_str(n[0])
lines=text.split('\n')
# 找 "The integration is off by default" 的行，打印前后 12 行
for idx,l in enumerate(lines):
    if 'integration is off by default' in l or 'Once enabled, the assistant' in l or 'Trilium supports four different types of providers' in l:
        print(f'===== 行 {idx} =====')
        for j in range(max(0,idx-4), min(len(lines),idx+9)):
            print(f'{j:4d}| {lines[j][:150]}')
        print()
