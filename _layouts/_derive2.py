# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# ===== 数据准备 =====
# 1. seg_map: 树路径段 -> docName 段
seg_map=json.load(open('_seg_map.json',encoding='utf-8'))
segmap={}
for k,v in seg_map.items():
    lvl,tseg=k.split('|',1)
    segmap[(int(lvl),tseg)]=v

# 2. help docName 全集 + 按父路径分组
help_rows=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
all_docnames=sorted(set(dn for _,dn in help_rows))
# 父路径 -> [docName]（docName 去掉末段后作为父）
from collections import defaultdict
doc_by_parent=defaultdict(list)
for dn in all_docnames:
    segs=dn.split('/')
    parent='/'.join(segs[:-1])
    doc_by_parent[parent].append(segs[-1])
print('help docName 总数:', len(all_docnames), '| 父路径种类:', len(doc_by_parent))

# 3. 用户指南树
ug_ids={}
def collect(parent, depth=0, path=()):
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0", (parent,)).fetchall()
    for (nid,) in r:
        t=cur.execute("SELECT title FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
        title=t[0] if t else nid
        ug_ids[nid]=(title, depth, path)
        collect(nid, depth+1, path+(title,))
collect('yTRmjAPEV7Wm')

# 4. 无 docPath 的 text 笔记
no_doc=[]
for nid in ug_ids:
    title,depth,path=ug_ids[nid]
    r=cur.execute("SELECT type FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if r and r[0]=='text':
        r2=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        if r2[0]==0:
            no_doc.append((nid,title,depth,path))
print('无 docPath text 笔记:', len(no_doc))

# ===== 路径翻译 =====
def path_to_doc_segs(path):
    """把用户指南树路径(不含根)翻译为 docName 段列表"""
    out=[]
    for i,seg in enumerate(path):
        v=segmap.get((i,seg))
        if v and len(v)==1:
            out.append(v[0])
        else:
            # 尝试 (English) 后缀
            m=re.search(r'\(([^()]+)\)$', seg)
            if m:
                out.append(m.group(1))
            else:
                return None
    return out

def get_content(nid):
    r=cur.execute("SELECT blobId FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
    if not r: return ''
    b=cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[0],)).fetchone()
    c=b[0] if b else ''
    if isinstance(c, bytes): c=c.decode('utf-8','replace')
    return c or ''

# ===== 叶子匹配策略 =====
def match_leaf(nid, title, parent_doc_segs):
    """返回 (docName, 置信度, 方法)"""
    parent_doc='User Guide/User Guide/'+'/'.join(parent_doc_segs) if parent_doc_segs else 'User Guide/User Guide'
    candidates=doc_by_parent.get(parent_doc, [])
    if not candidates:
        return None
    # 策略1: 标题括号内英文
    m=re.match(r'^(.*?)\s*\(([^()]+)\)$', title.strip())
    if m:
        eng=m.group(2).strip()
        for c in candidates:
            if c.lower()==eng.lower():
                return (parent_doc+'/'+c, 'HIGH', '英文括号匹配')
        # 相似（忽略大小写/空格）
        for c in candidates:
            if c.lower().replace(' ','')==eng.lower().replace(' ',''):
                return (parent_doc+'/'+c, 'HIGH', '英文括号匹配(宽松)')
    # 策略2: 标题本身是英文
    if re.match(r'^[A-Za-z0-9 _\-&\.\(\):]+$', title.strip()):
        for c in candidates:
            if c.lower()==title.strip().lower():
                return (parent_doc+'/'+c, 'HIGH', '标题即英文')
    # 策略3: 父目录只有一个候选
    if len(candidates)==1:
        return (parent_doc+'/'+candidates[0], 'MEDIUM', '父目录唯一候选')
    # 策略4: 候选唯一且包含 title 关键词（子串匹配，不区分大小写）
    t=title.strip().lower()
    hits=[c for c in candidates if t in c.lower() or c.lower() in t]
    if len(hits)==1:
        return (parent_doc+'/'+hits[0], 'MEDIUM', '子串匹配')
    return None

# ===== 执行 =====
results=[]
for nid,title,depth,path in no_doc:
    doc_segs=path_to_doc_segs(path[:-1])  # 父路径
    if doc_segs is None:
        # 尝试用完整路径（叶子也可能是段映射中的目录）
        doc_segs_full=path_to_doc_segs(path)
        if doc_segs_full:
            full='User Guide/User Guide/'+'/'.join(doc_segs_full)
            if full in all_docnames:
                results.append({'noteId':nid,'title':title,'docName':full,'conf':'HIGH','method':'整路径命中'})
                continue
        results.append({'noteId':nid,'title':title,'docName':None,'conf':None,'method':'路径无法翻译'})
        continue
    # 尝试整路径命中（叶子也在 docName 里）
    full_guess='User Guide/User Guide/'+'/'.join(list(doc_segs)+[path[-1]]) if path else 'User Guide/User Guide'
    # 叶子匹配
    r_match=match_leaf(nid, title, doc_segs)
    if r_match:
        dn,conf,method=r_match
        results.append({'noteId':nid,'title':title,'docName':dn,'conf':conf,'method':method})
    else:
        results.append({'noteId':nid,'title':title,'docName':None,'conf':None,'method':f'父目录{len(doc_segs)}段翻译OK,叶子未匹配,候选={doc_by_parent.get("User Guide/User Guide/"+"/".join(doc_segs),[])[:5]}'})

high=[r for r in results if r['conf']=='HIGH']
medium=[r for r in results if r['conf']=='MEDIUM']
none=[r for r in results if r['docName'] is None]
print(f'\n可确定(HIGH): {len(high)} | 中等(MEDIUM): {len(medium)} | 无法确定: {len(none)}')
print()
print('===== HIGH =====')
for r in high:
    print(f'  {r["noteId"]} | {r["title"]} => {r["docName"]} [{r["method"]}]')
print()
print('===== MEDIUM =====')
for r in medium:
    print(f'  {r["noteId"]} | {r["title"]} => {r["docName"]} [{r["method"]}]')
print()
print('===== 无法确定 =====')
for r in none:
    print(f'  {r["noteId"]} | {r["title"]} | {r["method"]}')

json.dump(results, open('_derive2.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
