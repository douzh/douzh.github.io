# -*- coding: utf-8 -*-
import sqlite3, json, re, difflib
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

help_content=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\help_content.json',encoding='utf-8'))

# 1. _help noteId -> docName
help_rows=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
help_docname={nid:dn for nid,dn in help_rows}
print('_help 节点:', len(help_docname))

# 2. 用户指南下所有 text 笔记内容
ug_ids=set()
def collect(parent):
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0", (parent,)).fetchall()
    for (nid,) in r:
        ug_ids.add(nid)
        collect(nid)
collect('yTRmjAPEV7Wm')

def get_content(nid):
    r=cur.execute("SELECT blobId FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if not r: return ''
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[0],)).fetchone()
    c=b[0] if b else ''
    if isinstance(c, bytes): c=c.decode('utf-8','replace')
    return c or ''

def norm_text(html):
    t=re.sub(r'<[^>]+>',' ', html)
    t=re.sub(r'&nbsp;?',' ', t)
    t=re.sub(r'&\w+;',' ', t)
    t=re.sub(r'#root/[_A-Za-z0-9]+',' ', t)
    t=re.sub(r'\s+',' ', t).strip().lower()
    return t

# 3. 对每个 help 节点生成规范化文本（用于匹配）
help_norm={}
for nid,dn in help_rows:
    c=help_content.get(nid,'')
    help_norm[nid]=norm_text(c)

# 4. 对 128 个无 docPath 笔记做匹配：取笔记内容前 200 字符的 4-gram 与各 help 内容比对
no_doc=[]
for nid in ug_ids:
    r=cur.execute("SELECT title,type FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if r and r[1]=='text':
        r2=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        if r2[0]==0:
            no_doc.append((nid, r[0]))

def chunks(s, n=4):
    s=s.replace(' ','')
    return {s[i:i+n] for i in range(0, max(1,len(s)-n+1), max(1,n//2))}

results=[]
for nid,title in no_doc:
    c=norm_text(get_content(nid))
    if len(c) < 40:
        results.append({'noteId':nid,'title':title,'match':None,'score':0,'reason':'内容过短'})
        continue
    cset=chunks(c[:400], 6)
    best=None; best_score=0
    for hid,hn in help_norm.items():
        if len(hn) < 60: continue
        hset=chunks(hn[:400], 6)
        inter=len(cset & hset)
        score=inter/max(1,len(cset))
        if score>best_score:
            best_score=score; best=hid
    results.append({'noteId':nid,'title':title,'match':best,'score':round(best_score,3),
                    'docName': help_docname.get(best,'') if best else ''})

matched=[r for r in results if r['match'] and r['score']>=0.25]
unmatched=[r for r in results if not (r['match'] and r['score']>=0.25)]
print(f'\n无 docPath 笔记: {len(no_doc)} | 内容匹配到 help: {len(matched)} | 未匹配: {len(unmatched)}')
print()
print('===== 内容匹配到的 =====')
for r in sorted(matched, key=lambda x:-x['score']):
    print(f"  {r['score']:.2f} | {r['noteId']} | {r['title']} => {r['docName']}")
print()
print('===== 未匹配的 =====')
for r in sorted(unmatched, key=lambda x:x['title']):
    print(f"  {r['noteId']} | {r['title']} | {r['reason'] if 'reason' in r else 'score='+str(r['score'])}")

json.dump(results, open('_ug_mapping_content.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
