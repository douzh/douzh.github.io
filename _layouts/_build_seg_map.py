# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 建立 noteId -> 树路径（用户指南内）
def get_path(nid):
    segs=[]
    cur2=nid
    while True:
        r=cur.execute("SELECT parentNoteId FROM branches WHERE noteId=? AND isDeleted=0 LIMIT 1", (cur2,)).fetchone()
        if not r: break
        p=r[0]
        if p=='yTRmjAPEV7Wm':
            segs.append('USER_GUIDE')
            break
        if p=='root':
            segs.append('ROOT')
            break
        r2=cur.execute("SELECT title FROM notes WHERE noteId=? AND isDeleted=0", (p,)).fetchone()
        segs.append(r2[0] if r2 else p)
        cur2=p
    return '/'.join(reversed(segs))

# 103 个已标笔记的树路径 vs docName
bil=cur.execute("SELECT n.noteId, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0 WHERE n.isDeleted=0").fetchall()
print('已标笔记:', len(bil))

# 用已标笔记建立 树路径分段 -> docName 分段 的对应（按层级）
# 每个已标笔记给出 (树路径 segs, docName segs)，同一层级可建立映射
# 树路径 segs = [USER_GUIDE, ...], docName segs = [User Guide, User Guide, ...]
pairs=[]
for nid,dn in bil:
    tp=get_path(nid).split('/')
    # 去掉开头的 USER_GUIDE
    while tp and tp[0] in ('USER_GUIDE','ROOT'): tp.pop(0)
    dn_segs=dn.split('/')
    # docName 通常是 "User Guide/User Guide/<...>"，去掉前两个
    while dn_segs and dn_segs[0]=='User Guide': dn_segs.pop(0)
    pairs.append((tp, dn_segs))

# 统计同层级的 中文段 -> 英文段 映射
# 同一父路径下，树段名 与 docName 段名 一一对应
seg_map={}  # (level_from_root, tree_seg) -> docName_seg
for tp, dn in pairs:
    for i, tseg in enumerate(tp):
        if i < len(dn):
            key=(i, tseg)
            seg_map.setdefault(key, set()).add(dn[i])
# 打印映射统计
print('\n===== 树路径段 -> docName 段 映射（样例） =====')
for (lvl, tseg), dnset in sorted(seg_map.items()):
    if len(dnset)==1:
        print(f'  L{lvl} {tseg!r} => {list(dnset)[0]!r}')
    else:
        print(f'  L{lvl} {tseg!r} => 多个: {dnset}')
json.dump({f'{k[0]}|{k[1]}':sorted(v) for k,v in seg_map.items()}, open('_seg_map.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
