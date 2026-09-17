# -*- coding: utf-8 -*-
"""查 25 个叶子的 docPath 属性 → 源 _help_ 节点，并读源内容前 200 字符"""
import sqlite3, json
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

leaves = ['aIwhuIM42UXZ','a3TKVvk1Jcuq','KENrlyFaUbZN','3GTPwnM0LXzq','I9TEAj9A5NIv',
          'sPaAXjVcvZZl','5HwfHJFwczl9','SEYvbzIDbgtV','fEvxRDgByx6J','aFYvSvzqySw1',
          'fupEm5MLR0eF','O4yjkaVH3YxV','o5byGpiYwkAf','74oHxA6XDX7Z','9H8FKhPVYLTX',
          'mWpKWRZZLu3R','3jBts3KWOAAP','PrgLiFypVLYw','eaZEEY2i4YT2','NwgMJByVeFyC',
          'Ht8ZUwhpA1DH','g0OYZmaGK1v8','v14qii09xWVc','z2cMreTyvWVz','6OOBk6hUKjLW']

out = {}
for nid in leaves:
    row = cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    if not row:
        print('%s 不存在!' % nid); continue
    title, typ, blobId = row
    attrs = cur.execute("SELECT name, value FROM attributes WHERE noteId=? AND isDeleted=0", (nid,)).fetchall()
    a = {n:v for n,v in attrs}
    docpath = a.get('docPath','')
    rec = {'title':title, 'type':typ, 'docPath':docpath}
    # 读源
    if docpath:
        src = cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (docpath,)).fetchone()
        if src:
            s = cur.execute("SELECT content FROM blobs WHERE blobId=?", (src[2],)).fetchone()
            rec['src_title'] = src[0]
            rec['src_type'] = src[1]
            rec['src_len'] = len(s[0]) if s else 0
        else:
            rec['src_title'] = '(库中无此节点)'
    out[nid] = rec
    print('%s | %s | docPath=%s | src=%s len=%s' % (nid, title[:40], docpath or '(无)', rec.get('src_title',''), rec.get('src_len','')))

with open('_leaf_docpath.json','w',encoding='utf-8') as f:
    json.dump(out, f, ensure_ascii=False, indent=1)
conn.close()
