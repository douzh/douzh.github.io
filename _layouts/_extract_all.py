# -*- coding: utf-8 -*-
"""批量提取双语叶子的中文部分，结果存 _extracted.json"""
import sqlite3, re, json
from _extract_cn2 import extract_cn

DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

def subtree(note_id):
    rows = cur.execute("""
      SELECT b.noteId, n.title, n.type, n.blobId
      FROM branches b JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId = ? ORDER BY b.notePosition, b.noteId
    """, (note_id,)).fetchall()
    out = []
    for r in rows:
        out.append(r)
        out += subtree(r[0])
    return out

targets = []
for root in ['39unKGhUFGBr','z5TN5NCWzga3']:
    for nid, title, typ, blobId in subtree(root):
        if typ != 'text':
            continue
        c = get_blob(blobId) or ''
        txt = re.sub(r'<[^>]+>', ' ', c)
        en = len(re.findall(r'[A-Za-z]{3,}', txt))
        cn = len(re.findall(r'[\u4e00-\u9fff]', txt))
        if not c.strip():
            continue
        targets.append({'noteId':nid, 'title':title, 'len':len(c), 'cn':cn, 'en':en})

print('候选叶子 %d 个' % len(targets))
result = {}
for t in targets:
    nid = t['noteId']
    row = cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = get_blob(row[0])
    res, err = extract_cn(c)
    if err:
        print('%s %s ERROR %s' % (nid, t['title'], err))
        continue
    rt = re.sub(r'<[^>]+>', ' ', res)
    r_en = len(re.findall(r'[A-Za-z]{3,}', rt))
    r_cn = len(re.findall(r'[\u4e00-\u9fff]', rt))
    t['out_len'] = len(res)
    t['out_cn'] = r_cn
    t['out_en'] = r_en
    result[nid] = res
    print('%s | %s | cn %d->%d | en %d->%d' % (nid, (t['title'] or '')[:40], t['cn'], r_cn, t['en'], r_en))

with open('_extracted.json','w',encoding='utf-8') as f:
    json.dump({'targets':targets,'content':result}, f, ensure_ascii=False, indent=1)
print('已保存 _extracted.json')
conn.close()
