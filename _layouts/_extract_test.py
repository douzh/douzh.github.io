# -*- coding: utf-8 -*-
# 快速验证: 提取单篇内容测试
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_str(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    c=r[0]
    if isinstance(c,bytes):
        try: c=c.decode('utf-8','replace')
        except: return ''
    return c

def is_cn(s):
    cjk=sum(1 for ch in s if '\u4e00'<=ch<='\u9fff')
    return cjk>=2

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

def extract_cn(text):
    lines=text.split('\n')
    out=[]
    i=0
    orphan=[]
    steps=0
    while i<len(lines) and steps<len(lines)*3:
        steps+=1
        line=lines[i]
        t=get_type(line)
        if t=='code':
            out.append(line); i+=1
            while i<len(lines) and not lines[i].lstrip().startswith('```'):
                out.append(lines[i]); i+=1
            if i<len(lines): out.append(lines[i]); i+=1
            continue
        if t=='img':
            if i+1<len(lines):
                nxt=lines[i+1]
                if get_type(nxt)=='para' and is_cn(nxt):
                    alt=nxt.strip().strip('（）()')
                    m=re.match(r'!\[[^\]]*\]\(([^)]+)\)', line.strip())
                    if m:
                        out.append(f'![{alt}]({m.group(1)})')
                        i+=2; continue
            out.append(line); i+=1; continue
        if not is_cn(line):
            if i+1<len(lines) and is_cn(lines[i+1]) and get_type(lines[i+1])==t:
                out.append(lines[i+1]); i+=2; continue
            else:
                out.append(line)
                if line.strip() and not re.match(r'^\s*\|?[\s:|-]+\|?\s*$',line):
                    orphan.append((i,line[:100]))
                i+=1; continue
        else:
            out.append(line); i+=1
    return '\n'.join(out), orphan

# 单篇测试
nid='Q0A1Gg3pgIOP'
n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
raw=blob_str(n[0])
cn,orphan=extract_cn(raw)
with open('_extract_test.txt','w',encoding='utf-8') as f:
    f.write('=== ORPHAN ===\n')
    for ln,txt in orphan:
        f.write(f'[{ln}] {txt}\n')
    f.write('\n=== CN ===\n')
    f.write(cn)
print('done len', len(cn), 'orphan', len(orphan))
