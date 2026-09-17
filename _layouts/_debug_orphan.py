# -*- coding: utf-8 -*-
import sqlite3, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
def blob_str(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    c=r[0]
    return c if isinstance(c,str) else c.decode('utf-8','replace')

def is_cn(s):
    return sum(1 for ch in s if '\u4e00'<=ch<='\u9fff')>=2

def get_type(line):
    s=line.lstrip()
    if s.startswith('```'): return 'code'
    if s.startswith('####'): return 'h4'
    if s.startswith('###'): return 'h3'
    if s.startswith('##'): return 'h2'
    if s.startswith('#'): return 'h1'
    if s.startswith('|'): return 'table'
    if s.startswith('*') or s.startswith('-') or re.match(r'^\d+\.',s): return 'list'
    if s.startswith('>'): return 'quote'
    if s.startswith('!['): return 'img'
    return 'para'

n=cur.execute("SELECT blobId FROM notes WHERE noteId='s5yEiZdU32Oh'",).fetchone()
text=blob_str(n[0])
lines=text.split('\n')
for idx in [3,6]:
    line=lines[idx]
    t=get_type(line)
    print(f'行{idx}: type={t} cn={is_cn(line)} text={line[:60]!r}')
    print(f'行{idx+1}: type={get_type(lines[idx+1])} cn={is_cn(lines[idx+1])} text={lines[idx+1][:60]!r}')
    print()
# 统计所有孤儿行并打印内容
i=0; orphans=[]
while i<len(lines):
    line=lines[i]; t=get_type(line)
    if t=='code':
        i+=1
        while i<len(lines) and not lines[i].lstrip().startswith('```'): i+=1
        if i<len(lines): i+=1
        continue
    if t=='img':
        i+=1; continue
    if not is_cn(line):
        j=i; block=[]
        while j<len(lines) and get_type(lines[j])==t and not is_cn(lines[j]):
            block.append(lines[j]); j+=1
        k=j; cn_block=[]
        while k<len(lines) and get_type(lines[k])==t and is_cn(lines[k]):
            cn_block.append(lines[k]); k+=1
        n=min(len(block),len(cn_block))
        for m in range(n, len(block)):
            if block[m].strip(): orphans.append((i+m, block[m][:80]))
        i=k
    else:
        i+=1
print('孤儿总数:', len(orphans))
for o in orphans[:40]:
    print('  ', o[1])
