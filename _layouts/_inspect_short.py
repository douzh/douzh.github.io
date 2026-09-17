# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

short_ids=['Ax2xW59Ff3jb','iyj7sZ20qUw8','BROicBbRhY1u','yzyWwxCeBHm2','hWMN4ej0xMh3','P5P0lLNUT9eC','jFXOYH8BO8zS','k8g7PA6MK9oa','VugMPu4jc7IZ','G6uh5oxvadSV','5JQ4vkZqncQO','GDf62xzQAPH5','20c3yUXh3jcZ','4aQnfnnr9AKb','4hBLB6wTZ1Zv']

for nid in short_ids:
    r=cur.execute("SELECT title FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    title=r[0] if r else '?'
    r2=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r2[0],)).fetchone()
    c=b[0] if b else ''
    if isinstance(c,bytes): c=c.decode('utf-8','replace')
    text=re.sub(r'<[^>]+>',' ',c)
    text=re.sub(r'\s+',' ',text).strip()
    print(f'{nid} | {title}')
    print(f'    len={len(c)} | 内容: {text[:200]}')
