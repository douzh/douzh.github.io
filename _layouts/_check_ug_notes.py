# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 1. 用户指南 book 子树
def subtree(parent, depth=0, out=None):
    if out is None: out=[]
    rows=cur.execute("""SELECT n.noteId,n.title,n.type,n.mime FROM branches b
        JOIN notes n ON n.noteId=b.noteId AND n.isDeleted=0
        WHERE b.parentNoteId=? AND b.isDeleted=0 ORDER BY b.notePosition""", (parent,)).fetchall()
    for r in rows:
        out.append((depth,)+r)
        subtree(r[0], depth+1, out)
    return out

print('===== 用户指南子树 =====')
for d,nid,title,typ,mime in subtree('yTRmjAPEV7Wm'):
    print('  '*d + f'{nid} [{typ}/{mime}] {title}')

# 2. 双语笔记数量与 docPath 标签
rows=cur.execute("SELECT COUNT(*) FROM attributes WHERE name='docPath' AND isDeleted=0").fetchone()
print('\ndocPath 标签数:', rows[0])

# 3. 双语笔记列表（含 docPath）
print('\n===== 双语笔记 (noteId | title | docPath) =====')
rows=cur.execute("""SELECT n.noteId, n.title, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0
    WHERE n.isDeleted=0 ORDER BY n.title""").fetchall()
for nid,title,dp in rows:
    print(f'{nid} | {title} | {dp}')
