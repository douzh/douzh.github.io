# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_len(blob_id):
    if not blob_id: return 0
    r=cur.execute("SELECT LENGTH(content) FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    return r[0] if r else 0

# 1) _help 节点类型分布（所有 _help_ 前缀）
r=cur.execute("SELECT type, COUNT(*) FROM notes WHERE noteId LIKE '_help_%' AND isDeleted=0 GROUP BY type").fetchall()
print('_help 节点类型分布:', r)

# 2) _help 有 docName 的节点类型分布
r=cur.execute("""SELECT n.type, COUNT(*) FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0 GROUP BY n.type""").fetchall()
print('_help 有 docName 的类型分布:', r)

# 3) 用户指南 tree 完整重建（含父路径）
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

# 4) _help docName -> 节点信息
help_map={}
r=cur.execute("""SELECT n.noteId, n.title, n.type, n.blobId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0""").fetchall()
for nid,title,ntype,blob,dn in r:
    help_map[dn]={'noteId':nid,'title':title,'type':ntype,'bl':blob_len(blob)}

# 5) 判定问题: 用户指南 book 或空text, 但源 docName 节点是 doc/text 且有 asar 内容
#    源 doc 类型: 内容在 asar, blob=0 但一定有 html; text 类型: blob>0
#    asar 中 docName 对应 html 都存在(之前验证过), 所以 doc 类型即"有内容"
problems=[]
for item in ug:
    dp=item['docPath']
    if not dp: continue
    src=help_map.get(dp)
    if not src: continue
    # 源有内容: doc 类型(asar html 存在) 或 text 且 blob>0
    if src['type']=='doc' or (src['type']=='text' and src['bl']>0):
        if item['type']=='book' or (item['type']=='text' and item['bl']==0):
            problems.append({**item, 'src_type':src['type'],'src_title':src['title'],'src_bl':src['bl']})

print('\n问题笔记数:', len(problems))
json.dump(problems, open('_mid_problems.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)

# 打印: 按 docPath 排序
problems.sort(key=lambda x:x['docPath'])
for p in problems:
    flag = 'book(网格/列表)' if p['type']=='book' else '空text'
    print(f"[{flag:14s} bl={p['bl']:5d}] {p['path']}  <<源>> {p['src_title']} [{p['src_type']} src_bl={p['src_bl']}]  :: {p['docPath']}")

# 按源目录分组统计
from collections import Counter
tops=Counter(p['docPath'].split('/')[2] if len(p['docPath'].split('/'))>2 else p['docPath'] for p in problems)
print('\n按章节分组:')
for k,v in sorted(tops.items()):
    print(f'  {k}: {v}')
