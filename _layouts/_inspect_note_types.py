# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_len(blob_id):
    if not blob_id: return 0
    r=cur.execute("SELECT LENGTH(content) FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    return r[0] if r else 0

# 找用户指南中标题含"笔记类型"的节点
r=cur.execute("""SELECT n.noteId, n.title, n.type, n.blobId FROM notes n
    JOIN branches b ON b.noteId=n.noteId
    WHERE n.isDeleted=0 AND n.title LIKE '%笔记类型%'""").fetchall()
for nid,title,ntype,blob in r:
    print(f'UG: {nid} | {title} | {ntype} | blob_len={blob_len(blob)}')

# 源: Note Types 节点
r=cur.execute("""SELECT n.noteId, n.title, n.type, n.blobId FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0 AND a.value LIKE '%Note Types%' AND a.value NOT LIKE '%/%/%/%'""").fetchall()
for nid,title,ntype,blob in r:
    print(f'HELP: {nid} | {title} | {ntype} | blob_len={blob_len(blob)}')
