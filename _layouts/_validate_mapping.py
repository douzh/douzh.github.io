# -*- coding: utf-8 -*-
import sqlite3, json, struct
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 1. docName 全集
help_rows=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
all_docnames=set(dn for _,dn in help_rows)

# 2. asar html 文件清单
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
def walk(node,p='',out=None):
    if out is None: out=[]
    if isinstance(node,dict):
        if 'files' in node and isinstance(node['files'],dict):
            for n,s in node['files'].items():
                walk(s,p+'/'+n,out)
        elif 'offset' in node:
            out.append(p)
    return out
htmls=set(walk(h['files'].get('assets',{}).get('files',{}).get('doc_notes',{}),'assets/doc_notes'))

# 3. 128 无 docPath 笔记集合
ug_ids=set()
def collect(parent):
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0", (parent,)).fetchall()
    for (nid,) in r:
        ug_ids.add(nid)
        collect(nid)
collect('yTRmjAPEV7Wm')
no_doc=set()
for nid in ug_ids:
    r=cur.execute("SELECT type FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if r and r[0]=='text':
        r2=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        if r2[0]==0:
            no_doc.add(nid)
print('无 docPath 笔记:', len(no_doc))

# 4. 校验映射
mapping=json.load(open('_manual_mapping.json',encoding='utf-8'))
print('映射条目:', len(mapping))

# 4a. 映射中的 noteId 是否都属于 no_doc（或已标）
already=set(mapping)& (ug_ids-no_doc)
print('映射里已标 docPath 的(无需再加):', len(already), already)

# 4b. no_doc 中未覆盖的
uncovered=no_doc-set(mapping)
print('\nno_doc 中未被映射覆盖:', len(uncovered))
for nid in sorted(uncovered):
    r=cur.execute("SELECT title FROM notes WHERE noteId=?", (nid,)).fetchone()
    print('  ', nid, r[0] if r else '?')

# 4c. docName 不存在于全集
bad=[]
for nid,dn in mapping.items():
    if dn not in all_docnames:
        bad.append((nid,dn))
print('\ndocName 不在全集的:', len(bad))
for nid,dn in bad:
    r=cur.execute("SELECT title FROM notes WHERE noteId=?", (nid,)).fetchone()
    print('  ', nid, r[0] if r else '?', '=>', dn)

# 4d. asar html 缺失
missing_html=[]
for nid,dn in mapping.items():
    if dn in all_docnames:
        p='assets/doc_notes/en/'+dn+'.html'
        if p not in htmls:
            missing_html.append((nid,dn))
print('\nasar html 缺失:', len(missing_html))
for nid,dn in missing_html:
    r=cur.execute("SELECT title FROM notes WHERE noteId=?", (nid,)).fetchone()
    print('  ', nid, r[0] if r else '?', '=>', dn)
