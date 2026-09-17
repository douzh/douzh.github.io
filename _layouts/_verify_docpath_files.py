# -*- coding: utf-8 -*-
import sqlite3, json, struct, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 1. asar 中 doc_notes/en 下所有文件清单
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
def walk(node,p='',out=None):
    if out is None: out=[]
    if isinstance(node,dict):
        if 'files' in node and isinstance(node['files'],dict):
            for n,s in node['files'].items():
                walk(s,p+'/'+n,out)
        elif 'offset' in node:
            out.append((p,int(node['offset']),node.get('size')))
    return out
# 只取 doc_notes/en 分支
dn_root=h['files'].get('assets',{}).get('files',{}).get('doc_notes',{})
files=[]
def walk_doc(node,p=''):
    if isinstance(node,dict):
        if 'files' in node and isinstance(node['files'],dict):
            for n,s in node['files'].items():
                walk_doc(s,p+'/'+n)
        elif 'offset' in node:
            files.append((p,int(node['offset']),node.get('size')))
walk_doc(dn_root, 'assets/doc_notes')
doc_files=[f[0] for f in files]
html_files=[f for f in doc_files if f.endswith('.html')]
print('doc_notes 总文件:', len(doc_files), '| html:', len(html_files))

# 2. 103 个 docPath -> 检查对应 html 是否存在
bil=cur.execute("SELECT n.noteId, n.title, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0 WHERE n.isDeleted=0").fetchall()
print('带 docPath 笔记:', len(bil))
missing=[]
for nid,title,dn in bil:
    html_path='assets/doc_notes/en/'+dn+'.html'
    if html_path not in doc_files:
        missing.append((nid,title,dn,html_path))
print('docPath 对应 html 缺失:', len(missing))
for m in missing:
    print('  MISSING:', m[0], m[1], '=>', m[3])
    # 尝试找相似
    cand=[f for f in html_files if f.split('/')[-1]==m[3].split('/')[-1]]
    print('    相似候选:', cand[:3])

# 3. 重复 docPath
from collections import Counter
c=Counter(r[2] for r in bil)
print('\n重复 docPath:', {k:v for k,v in c.items() if v>1})
