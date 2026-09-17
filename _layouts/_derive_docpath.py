# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

seg_map=json.load(open('_seg_map.json',encoding='utf-8'))
# 解析回 (lvl, tree_seg) -> [doc segs]
segmap={}
for k,v in seg_map.items():
    lvl,tseg=k.split('|',1)
    segmap[(int(lvl),tseg)]=v

def get_path(nid):
    segs=[]
    cur2=nid
    while True:
        r=cur.execute("SELECT parentNoteId FROM branches WHERE noteId=? AND isDeleted=0 LIMIT 1", (cur2,)).fetchone()
        if not r: break
        p=r[0]
        if p in ('yTRmjAPEV7Wm','root'):
            break
        r2=cur.execute("SELECT title FROM notes WHERE noteId=? AND isDeleted=0", (p,)).fetchone()
        segs.append(r2[0] if r2 else p)
        cur2=p
    return list(reversed(segs))

def seg_to_doc(segs):
    # 逐段翻译：每段用 (level, seg) 查映射；若查不到且段内含括号英文，用括号英文
    out=[]
    for i,seg in enumerate(segs):
        v=segmap.get((i,seg))
        if v and len(v)==1:
            out.append(v[0])
        else:
            # 尝试从 "中文(English)" 提取英文
            m=re.search(r'\(([^()]+)\)$', seg)
            if m:
                out.append(m.group(1))
            else:
                return None
    return out

# 所有 help docName 末段（作为叶子标题候选）
help_rows=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'").fetchall()
docname_last={}
for nid,dn in help_rows:
    ls=dn.split('/')[-1]
    docname_last.setdefault(ls.lower(), []).append(dn)

# 用户指南下无 docPath 的 text 笔记
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

def strip_cn(title):
    m=re.match(r'^(.*?)\s*\(([^()]*)\)$', title.strip())
    if m: return m.group(1).strip(), m.group(2).strip()
    return title.strip(), None

results=[]
for nid,title in no_doc:
    base, eng = strip_cn(title)
    path=get_path(nid)
    dnpath=seg_to_doc(path)
    leaf=None
    if eng:
        cand=docname_last.get(eng.lower(),[])
        leaf=cand[0] if cand else None
    if leaf is None:
        cand=docname_last.get(base.lower(),[])
        if cand: leaf=cand[0]
    if leaf is None and eng:
        # 尝试直接匹配 docName 末段（不区分大小写）
        for ls, dns in docname_last.items():
            if ls==eng.lower() or ls.replace(' ','')==eng.lower().replace(' ',''):
                leaf=dns[0]; break
    if dnpath is None:
        results.append({'noteId':nid,'title':title,'derived':None,'leaf_match':leaf,'status':'路径无法推导'})
    else:
        parent='/'.join(['User Guide','User Guide']+dnpath)
        if leaf:
            results.append({'noteId':nid,'title':title,'derived':f'{parent}/{leaf}','leaf_match':leaf,'status':'OK'})
        else:
            # 无叶子：可能是 book 目录本身（如 FAQ、Feature Highlights 是叶子但标题不同）
            results.append({'noteId':nid,'title':title,'derived':parent,'leaf_match':None,'status':'路径推导+无叶子匹配'})

ok=[r for r in results if r['status']=='OK']
print(f'无 docPath 笔记: {len(no_doc)} | 完整推导: {len(ok)}')
print()
print('===== 完整推导的 =====')
for r in ok:
    print(f'  {r["noteId"]} | {r["title"]}\n      => {r["derived"]}')
print()
print('===== 未完整推导的 =====')
for r in [x for x in results if x['status']!='OK']:
    print(f'  {r["noteId"]} | {r["title"]} | {r["status"]} | leaf={r["leaf_match"]} | derived={r["derived"]}')

json.dump(results, open('_ug_derived.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
