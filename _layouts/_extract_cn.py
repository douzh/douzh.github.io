# -*- coding: utf-8 -*-
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
    # 含 CJK 字符比例
    cjk=sum(1 for ch in s if '\u4e00'<=ch<='\u9fff')
    return cjk>=2  # 至少2个汉字

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
    orphan=[]  # 英文孤儿行（无配对中文）
    while i<len(lines):
        line=lines[i]
        t=get_type(line)
        if t=='code':
            # 代码块整体保留
            out.append(line); i+=1
            while i<len(lines) and not lines[i].lstrip().startswith('```'):
                out.append(lines[i]); i+=1
            if i<len(lines): out.append(lines[i]); i+=1
            continue
        if t=='img':
            if i+1<len(lines):
                nxt=lines[i+1]
                # 图片对: ![](file) + (图片：中文说明)
                if get_type(nxt)=='para' and is_cn(nxt):
                    alt=nxt.strip().strip('（）()')
                    m=re.match(r'!\[[^\]]*\]\(([^)]+)\)', line.strip())
                    if m:
                        out.append(f'![{alt}]({m.group(1)})')
                        i+=2; continue
            out.append(line); i+=1; continue
        # 非中文行: 看下一行是否中文配对
        if not is_cn(line):
            if i+1<len(lines) and is_cn(lines[i+1]) and get_type(lines[i+1])==t:
                out.append(lines[i+1]); i+=2; continue
            else:
                # 孤行: 保留原样（可能本身就是单语/代码/分隔行）
                out.append(line)
                if t=='table' and line.strip()=='|---|' or re.match(r'^\s*\|?[\s:|-]+\|?\s*$',line) and t=='table':
                    pass
                elif line.strip() and not line.strip().startswith('|---'):
                    orphan.append((i,line[:120]))
                i+=1; continue
        else:
            out.append(line); i+=1
    return '\n'.join(out), orphan

issues=json.load(open('_book_has_content.json',encoding='utf-8'))
print('处理笔记:', len(issues))
all_orphan={}
results={}
for it in issues:
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (it['book_noteId'],)).fetchone()
    raw=blob_str(n[0])
    cn,orphan=extract_cn(raw)
    results[it['book_noteId']]={'title':it['book_title'],'cn':cn,'raw_len':len(raw),'cn_len':len(cn)}
    if orphan:
        all_orphan[it['book_noteId']]={'title':it['book_title'],'orphans':orphan}
json.dump(results, open('_cn_extract.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)

print('\n=== 含英文孤儿行的笔记 ===')
for nid,v in all_orphan.items():
    print(f"{v['title']}: {len(v['orphans'])} 行")
    for ln,txt in v['orphans'][:4]:
        print(f'    [{ln}] {txt}')
