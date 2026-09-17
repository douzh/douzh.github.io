# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 总 docPath 数
rows=cur.execute("SELECT COUNT(*) FROM attributes WHERE name='docPath' AND isDeleted=0").fetchone()
print('docPath 标签总数:', rows[0])

# 有 docPath 的笔记数（去重）
rows=cur.execute("SELECT COUNT(DISTINCT noteId) FROM attributes WHERE name='docPath' AND isDeleted=0").fetchone()
print('有 docPath 的笔记数:', rows[0])

# 用户指南下 text 笔记的 docPath 覆盖
ug_ids=set()
def collect(parent):
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0", (parent,)).fetchall()
    for (nid,) in r:
        ug_ids.add(nid)
        collect(nid)
collect('yTRmjAPEV7Wm')
text_total=0; text_with=0; text_without=[]
for nid in ug_ids:
    r=cur.execute("SELECT type FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if r and r[0]=='text':
        text_total+=1
        r2=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        if r2[0]>0: text_with+=1
        else: text_without.append(nid)
print(f'用户指南 text 笔记: {text_total} | 有 docPath: {text_with} | 无 docPath: {len(text_without)}')
for nid in text_without:
    r=cur.execute("SELECT title FROM notes WHERE noteId=?", (nid,)).fetchone()
    print('  无标签:', nid, r[0] if r else '?')

# 抽样验证新标签值
print('\n抽样新标签:')
rows=cur.execute("""SELECT n.noteId, n.title, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0
    WHERE n.noteId IN ('6at3wkEnLIfu','NRzmLnGiDb8U','eT4hTrNrfbR0','Td5XP4paGGvm','5HwfHJFwczl9','qfffwa20agq7')""").fetchall()
for r in rows: print(' ', r)
