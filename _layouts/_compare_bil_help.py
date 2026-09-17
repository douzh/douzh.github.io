# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 1. 全部双语笔记（带 docPath）
rows=cur.execute("""SELECT n.noteId, n.title, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0
    WHERE n.isDeleted=0 ORDER BY a.value""").fetchall()
print('双语笔记总数:', len(rows))

# 2. 全部 _help 源节点的 docName
rows2=cur.execute("""SELECT n.noteId, n.title, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%' ORDER BY a.value""").fetchall()
print('_help 源节点 docName 总数:', len(rows2))

# 3. 对比：双语 docPath 集合 vs help docName 集合
bil_docs=set(r[2] for r in rows)
help_docs=set(r[2] for r in rows2)
print('\n双语 docPath 唯一值:', len(bil_docs))
print('help docName 唯一值:', len(help_docs))
print('双语∩help 交集:', len(bil_docs & help_docs))
print('双语有但 help 没有(无法对应):', len(bil_docs - help_docs))
for d in sorted(bil_docs - help_docs):
    print('   [双语独有]', d)
print('help 有但双语没有(未翻译):', len(help_docs - bil_docs))

# 4. 检查重复 docPath（一个 docName 对应多个笔记）
from collections import Counter
c=Counter(r[2] for r in rows)
dups={k:v for k,v in c.items() if v>1}
print('\n重复 docPath:', len(dups))
for k,v in dups.items():
    print('   ', v, k)

# 5. 无 docPath 的 text 笔记（用户指南下可能漏标的）
ug_ids=set()
def collect(parent):
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0", (parent,)).fetchall()
    for (nid,) in r:
        ug_ids.add(nid)
        collect(nid)
collect('yTRmjAPEV7Wm')
no_doc=[]
for nid in ug_ids:
    r=cur.execute("SELECT title,type FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if r and r[1]=='text':
        r2=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        if r2[0]==0:
            no_doc.append((nid, r[0]))
print('\n用户指南下无 docPath 的 text 笔记:', len(no_doc))
for nid,t in sorted(no_doc, key=lambda x:x[1]):
    print('   ', nid, t)

json.dump({'bil_notes':[{'noteId':r[0],'title':r[1],'docPath':r[2]} for r in rows]}, open('_bil_full.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
json.dump({'help_notes':[{'noteId':r[0],'title':r[1],'docName':r[2]} for r in rows2]}, open('_help_full.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
