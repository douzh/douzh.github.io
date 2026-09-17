# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_len(blob_id):
    if not blob_id: return 0
    r=cur.execute("SELECT LENGTH(content) FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    return r[0] if r else 0

# 用户指南树
def build_tree(parent, path=''):
    rows=[]
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0 ORDER BY notePosition", (parent,)).fetchall()
    for (nid,) in r:
        n=cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
        if not n: continue
        title=n[0]; ntype=n[1]; bl=blob_len(n[2])
        dp=cur.execute("SELECT value FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        dp=dp[0] if dp else None
        p=path+'/'+title
        rows.append({'noteId':nid,'title':title,'type':ntype,'bl':bl,'docPath':dp,'path':p})
        rows.extend(build_tree(nid,p))
    return rows
ug=build_tree('yTRmjAPEV7Wm')

# _help: docName -> node
help_map={}
r=cur.execute("""SELECT n.noteId, n.title, n.type, n.blobId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0""").fetchall()
for nid,title,ntype,blob,dn in r:
    help_map[dn]={'noteId':nid,'title':title,'type':ntype,'bl':blob_len(blob)}

# 分析所有 book 节点
books=[b for b in ug if b['type']=='book']
print('book 节点数:', len(books))

def children_of(nid):
    return [b for b in ug if b.get('_parent')==nid]

# 建立父子映射
by_id={b['noteId']:b for b in ug}
for b in ug:
    b['_kids']=[]
# 用 branches 建父子
for nid in [b['noteId'] for b in ug]:
    r=cur.execute("SELECT parentNoteId FROM branches WHERE noteId=? AND isDeleted=0", (nid,)).fetchall()
    for (pid,) in r:
        if pid in by_id:
            by_id[pid].setdefault('_kids',[]).append(by_id[nid])

print('\n=== 用户指南 book 节点 vs 源 docName 路径 ===')
issues=[]
for b in books:
    kids=b.get('_kids',[])
    kid_dps=[k['docPath'] for k in kids if k['docPath']]
    # book 自身或子节点的 docPath 目录
    dp_dir=None
    if kid_dps:
        dp_dir='/'.join(kid_dps[0].split('/')[:-1])
    elif b['docPath']:
        dp_dir=b['docPath']
    src=None
    if dp_dir and dp_dir in help_map:
        src=help_map[dp_dir]
    print(f"\n[{b['noteId']}] {b['title']}  (bl={b['bl']}, kids={len(kids)})")
    print(f"   子docPath目录: {dp_dir}")
    if src:
        print(f"   源: {src['title']} [{src['type']} bl={src['bl']}]  -> {'【有内容但转换丢失】' if src['type']=='doc' else ''}")
        if src['type']=='doc':
            issues.append({'book_noteId':b['noteId'],'book_title':b['title'],'book_bl':b['bl'],
                           'src_docName':dp_dir,'src_title':src['title']})
    else:
        print(f"   源: 无对应 docName 节点")

print('\n\n=== 问题汇总: book 对应源 doc(有内容) ===')
print('数量:', len(issues))
json.dump(issues, open('_book_issues.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
for i in issues:
    print(f"  {i['book_title']}  (bl={i['book_bl']})  <<源>> {i['src_title']}  :: {i['src_docName']}")
