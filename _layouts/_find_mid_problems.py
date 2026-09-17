# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# blob 内容长度辅助（blob 表存内容）
def blob_len(blob_id):
    if not blob_id: return 0
    r=cur.execute("SELECT LENGTH(content) FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    return r[0] if r else 0

# 1) 用户指南树: 每个笔记 (title, type, blob_len, docPath)
def build_tree(parent):
    rows=[]
    r=cur.execute("SELECT noteId FROM branches WHERE parentNoteId=? AND isDeleted=0 ORDER BY notePosition", (parent,)).fetchall()
    for (nid,) in r:
        n=cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=? AND isDeleted=0", (nid,)).fetchone()
        if not n: continue
        title=n[0]; ntype=n[1]; bl=blob_len(n[2])
        dp=cur.execute("SELECT value FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (nid,)).fetchone()
        rows.append((nid,title,ntype,bl,dp[0] if dp else None))
        rows.extend(build_tree(nid))
    return rows

ug=build_tree('yTRmjAPEV7Wm')
print('用户指南总笔记:', len(ug))

# 类型分布
from collections import Counter
print('用户指南类型分布:', Counter(t for _,_,t,_,_ in ug))

# 2) _help 节点: docName -> (title, type, blob_len)
help_map={}
r=cur.execute("""SELECT n.noteId, n.title, n.type, n.blobId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0""").fetchall()
for nid,title,ntype,blob,dn in r:
    help_map[dn]=(title,ntype,blob_len(blob))
print('_help docName 节点:', len(help_map))

# 3) 判定问题: 用户指南笔记有 docPath, 源是 text 且有内容, 但用户指南是 空text / grid / list
problems=[]
for nid,title,ntype,bl,dp in ug:
    if not dp: continue
    src=help_map.get(dp)
    if not src: continue
    s_title,s_type,s_bl=src
    if s_type=='text' and s_bl>0:
        if ntype!='text' or (ntype=='text' and bl==0):
            problems.append((nid,title,ntype,bl,dp,s_title,s_type,s_bl))
print('\n问题笔记数:', len(problems))

# 输出成 json + 文本清单（带父路径）
json.dump(problems, open('_mid_problems.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)

# 打印目录（按 docPath 排序，便于对照）
problems.sort(key=lambda x:x[4])
for nid,title,ntype,bl,dp,s_title,s_type,s_bl in problems:
    flag = '空text' if (ntype=='text' and bl==0) else ntype
    print(f'[{flag:6s} len={bl:5d}] {title}  <<源>> {s_title} [{s_type} len={s_bl}]  :: {dp}')
