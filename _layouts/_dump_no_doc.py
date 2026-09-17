# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

help_rows=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
all_docnames=sorted(set(dn for _,dn in help_rows))

# 用户指南树
ug_ids={}
def collect(parent, path=()):
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0", (parent,)).fetchall()
    for (nid,) in r:
        t=cur.execute("SELECT title FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
        title=t[0] if t else nid
        ug_ids[nid]=(title, path)
        collect(nid, path+(title,))
collect('yTRmjAPEV7Wm')

no_doc=[]
for nid in ug_ids:
    title,path=ug_ids[nid]
    r=cur.execute("SELECT type FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if r and r[0]=='text':
        r2=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        if r2[0]==0:
            no_doc.append((nid,title,path))

def get_content(nid):
    r=cur.execute("SELECT blobId FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if not r: return ''
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[0],)).fetchone()
    c=b[0] if b else ''
    if isinstance(c, bytes): c=c.decode('utf-8','replace')
    return c or ''

out=[]
for nid,title,path in sorted(no_doc, key=lambda x:x[1]):
    c=get_content(nid)
    text=re.sub(r'<[^>]+>',' ',c)
    text=re.sub(r'&nbsp;?',' ',text)
    text=re.sub(r'&\w+;',' ',text)
    text=re.sub(r'\s+',' ',text).strip()
    out.append({
        'noteId':nid,'title':title,'path':'/'.join(path),
        'len':len(c),'text':text[:220],
        'enwords':re.findall(r'[A-Za-z][A-Za-z\-\. ]{2,}', text)[:8]
    })
json.dump(out, open('_no_doc_full.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
# 同时输出 docName 全集供参考
json.dump(all_docnames, open('_docnames_all.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('输出完成:', len(out), '条笔记;', len(all_docnames), '个docName')
