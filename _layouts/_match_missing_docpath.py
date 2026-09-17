# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 用户指南下所有 text 笔记（无 docPath 的）
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

# 所有 help docName
help_rows=cur.execute("SELECT n.noteId, n.title, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
# docName 最后一段 = 标题（英文）
def last_seg(docname): return docname.split('/')[-1] if docname else ''
def norm(s):
    s=s.lower().strip()
    return s

# 用最后一段英文名做匹配（不区分大小写、去括号后缀）
doc_by_last={}
for nid,title,dn in help_rows:
    ls=norm(last_seg(dn))
    doc_by_last.setdefault(ls, []).append(dn)

def strip_cn(title):
    # 去掉 (中文) 后缀
    m=re.match(r'^(.*?)\s*\([^)]*\)$', title.strip())
    return m.group(1) if m else title

matched=[]; unmatched=[]
for nid,t in no_doc:
    base=strip_cn(t)
    hits=doc_by_last.get(norm(base), [])
    if hits:
        matched.append((nid,t,hits))
    else:
        unmatched.append((nid,t))

print('无 docPath 的 text 笔记总数:', len(no_doc))
print('能按标题末段匹配到 help docName:', len(matched))
print('匹配不到:', len(unmatched))
print()
print('===== 匹配到的（应为漏标 docPath）=====')
for nid,t,hits in sorted(matched, key=lambda x:x[1]):
    print(f'{nid} | {t} => {hits[0]}')
print()
print('===== 匹配不到的 =====')
for nid,t in sorted(unmatched, key=lambda x:x[1]):
    print(f'{nid} | {t}')
