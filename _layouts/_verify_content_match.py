# -*- coding: utf-8 -*-
import sqlite3, json, struct, re, html as htmlmod
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
def walk(node,p='',out=None):
    if out is None: out=[]
    if isinstance(node,dict):
        if 'files' in node and isinstance(node['files'],dict):
            for n,s in node['files'].items():
                walk(s,p+'/'+n,out)
        elif 'offset' in node:
            out.append((p,int(node['offset']),node.get('size')))
    return out
allf=walk(h['files'].get('assets',{}).get('files',{}).get('doc_notes',{}), 'assets/doc_notes')
offmap={p:(o,s) for p,o,s in allf}

def read_doc(path):
    if path not in offmap: return None
    o,s=offmap[path]
    return data[data_start+o:data_start+o+s].decode('utf-8','replace')

def norm(html):
    t=re.sub(r'<script[\s\S]*?</script>',' ', html, flags=re.I)
    t=re.sub(r'<style[\s\S]*?</style>',' ', t, flags=re.I)
    t=re.sub(r'<[^>]+>',' ', t)
    t=htmlmod.unescape(t)
    t=re.sub(r'\s+',' ', t).strip()
    return t

# 抽查 5 个带 docPath 笔记：比较英文源 vs 双语笔记中的英文/中文
bil=cur.execute("SELECT n.noteId, n.title, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0 WHERE n.isDeleted=0").fetchall()
samples=['3GTPwnM0LXzq','6at3wkEnLIfu','Y2SA0lctNgeT','EkPCJ5MeC1N6','sIfLV6itqwru']

for nid in samples:
    row=[r for r in bil if r[0]==nid]
    if not row: print(nid,'无 docPath?'); continue
    _,title,dn=row[0]
    src=read_doc('assets/doc_notes/en/'+dn+'.html')
    # 双语笔记内容
    r=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[0],)).fetchone()
    ncontent=b[0] if b else ''
    if isinstance(ncontent,bytes): ncontent=ncontent.decode('utf-8','replace')
    ntext=norm(ncontent)
    stext=norm(src) if src else ''
    print(f'=== {nid} | {title}')
    print(f'  docPath: {dn}')
    print(f'  源 html: {"存在" if src else "缺失"}, 源纯文本长度={len(stext)}')
    print(f'  双语笔记纯文本长度={len(ntext)}')
    # 中文占比
    cn=len(re.findall(r'[\u4e00-\u9fff]', ntext))
    en=len(re.findall(r'[A-Za-z]', ntext))
    print(f'  双语笔记 中文字符={cn}, 英文字符={en}')
    # 源前 100 字 vs 笔记前 100 字
    print(f'  源开头: {stext[:100]}')
    print(f'  笔记开头: {ntext[:100]}')
    print()
