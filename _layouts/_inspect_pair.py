# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_str(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    c=r[0]
    if isinstance(c,bytes):
        try: c=c.decode('utf-8','replace')
        except: return ''
    return c

# 看完整内容，分析英文/中文配对
for nid,label in [('Q0A1Gg3pgIOP','笔记类型'),('gBJUFdOLz9GA','脚本')]:
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c=blob_str(n[0])
    print(f'===== {label} ({len(c)} chars) =====')
    print(c[:3000])
    print('\n...\n')
