# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 汇总所有无 docPath 笔记的匹配证据
# 证据源1: 英文标题匹配 (35个) - 已保存的匹配
# 证据源2: 内容相似度匹配 (17个)
# 证据源3: 树路径推导 (部分)

# 重新做英文标题匹配（含括号英文）
help_rows=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
docname_last={}
for nid,dn in help_rows:
    docname_last.setdefault(dn.split('/')[-1].lower(), []).append(dn)

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

def get_content(nid):
    r=cur.execute("SELECT blobId FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if not r: return ''
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[0],)).fetchone()
    c=b[0] if b else ''
    if isinstance(c, bytes): c=c.decode('utf-8','replace')
    return c or ''

# 判定函数：无 docPath 的 text 笔记是否翻译自帮助文档
# 判据A: 标题括号内有英文，且英文与某 docName 末段匹配
# 判据B: 内容包含中文翻译特征 + 英文术语（即双语笔记）
# 判据C: 内容为空或过短（占位/参考页）
def classify(nid,title):
    base, eng=None, None
    m=re.match(r'^(.*?)\s*\(([^()]*)\)$', title.strip())
    if m:
        base=m.group(1).strip(); eng=m.group(2).strip()
        if eng and eng.lower() in docname_last:
            return ('MATCH_EN', docname_last[eng.lower()][0])
    c=get_content(nid)
    cn=len(re.findall(r'[\u4e00-\u9fff]', c))
    en=len(re.findall(r'[A-Za-z]', c))
    if cn>50 and en>30:
        return ('BILINGUAL', None)  # 中英混合 -> 翻译笔记
    if cn>80:
        return ('CHINESE_ONLY', None)  # 纯中文 -> 很可能翻译
    if len(c.strip())<200:
        return ('SHORT', None)  # 占位/参考
    return ('OTHER', None)

cats={'MATCH_EN':[], 'BILINGUAL':[], 'CHINESE_ONLY':[], 'SHORT':[], 'OTHER':[]}
for nid,title in no_doc:
    k,docname=classify(nid,title)
    cats[k].append((nid,title,docname))

total=len(no_doc)
print(f'无 docPath 的 text 笔记: {total}')
for k in ['MATCH_EN','BILINGUAL','CHINESE_ONLY','SHORT','OTHER']:
    v=cats[k]
    print(f'\n[{k}] {len(v)} 个')
    if k in ('MATCH_EN','SHORT','OTHER'):
        for nid,title,dn in v:
            print(f'    {nid} | {title}' + (f' => {dn}' if dn else ''))

json.dump({k:v for k,v in cats.items()}, open('_final_classify.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
