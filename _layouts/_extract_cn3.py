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

def is_list_related(line):
    """列表块: 列表项或缩进延续行"""
    if not line.strip(): return False
    if get_type(line)=='list': return True
    # 缩进4空格以上的文本（列表的延续）
    if line.startswith('    ') and line.strip(): return True
    return False

def is_html_img(line):
    s=line.strip()
    return s.startswith('<figure') or (s.startswith('<img'))

def extract_cn(text):
    lines=text.split('\n')
    out=[]
    i=0
    orphan=[]
    while i<len(lines):
        line=lines[i]
        t=get_type(line)
        if not line.strip():
            out.append(''); i+=1; continue
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
        if is_html_img(line):
            # HTML 图片: <figure><img src="xxx"></figure> + 中文说明
            if i+1<len(lines) and is_cn(lines[i+1]) and get_type(lines[i+1])=='para':
                alt=lines[i+1].strip().strip('（）()')
                m=re.search(r'src="([^"]+)"', line)
                if m:
                    out.append(f'![{alt}]({m.group(1)})')
                    i+=2; continue
            out.append(line); i+=1; continue
        if not is_cn(line):
            # 列表相关: 连续"列表/缩进"行作为一个块
            if is_list_related(line):
                j=i; block=[]
                while j<len(lines) and is_list_related(lines[j]) and not is_cn(lines[j]):
                    block.append(lines[j]); j+=1
                k=j; cn_block=[]
                while k<len(lines) and is_list_related(lines[k]) and is_cn(lines[k]):
                    cn_block.append(lines[k]); k+=1
                n=min(len(block),len(cn_block))
                for m in range(n): out.append(cn_block[m])
                for m in range(n,len(block)):
                    out.append(block[m])
                    if block[m].strip(): orphan.append((i+m, block[m][:100]))
                i=k; continue
            # 普通块: 连续同类型非中文行
            j=i; block=[]
            while j<len(lines) and lines[j].strip() and get_type(lines[j])==t and not is_cn(lines[j]):
                block.append(lines[j]); j+=1
            k=j; cn_block=[]
            while k<len(lines) and lines[k].strip() and get_type(lines[k])==t and is_cn(lines[k]):
                cn_block.append(lines[k]); k+=1
            n=min(len(block),len(cn_block))
            for m in range(n): out.append(cn_block[m])
            for m in range(n,len(block)):
                out.append(block[m])
                if block[m].strip(): orphan.append((i+m, block[m][:100]))
            i=k; continue
        else:
            out.append(line); i+=1
    return '\n'.join(out), orphan

issues=json.load(open('_book_has_content.json',encoding='utf-8'))
results={}
total_orphan=0
for it in issues:
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (it['book_noteId'],)).fetchone()
    raw=blob_str(n[0])
    cn,orphan=extract_cn(raw)
    results[it['book_noteId']]={'title':it['book_title'],'cn':cn,'raw_len':len(raw),'cn_len':len(cn),'orphans':len(orphan),'orphan_list':[o[1] for o in orphan[:15]]}
    total_orphan+=len(orphan)
json.dump(results, open('_cn_extract.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('total_orphan:', total_orphan)
with open('_cn_extract_summary.txt','w',encoding='utf-8') as f:
    for nid,v in results.items():
        f.write(f"{v['title']}: raw={v['raw_len']} cn={v['cn_len']} orphan={v['orphans']}\n")
        for o in v['orphan_list']:
            f.write(f"    ORPHAN: {o}\n")
print('done')
