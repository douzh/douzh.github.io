# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

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

# _help 节点：title + docName
help_rows=cur.execute("SELECT n.noteId, n.title, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
print('_help 节点数:', len(help_rows))
print()
print('===== 前 30 个 _help 节点 title vs docName 末段 =====')
for nid,t,dn in help_rows[:30]:
    print(f'  title={t!r} | docName末段={dn.split(chr(47))[-1]!r}')
