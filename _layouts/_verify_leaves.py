# -*- coding: utf-8 -*-
import sqlite3, re, json
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
leaves = ['aIwhuIM42UXZ','a3TKVvk1Jcuq','KENrlyFaUbZN','3GTPwnM0LXzq','I9TEAj9A5NIv',
          'sPaAXjVcvZZl','5HwfHJFwczl9','SEYvbzIDbgtV','fEvxRDgByx6J','aFYvSvzqySw1',
          'fupEm5MLR0eF','O4yjkaVH3YxV','o5byGpiYwkAf','74oHxA6XDX7Z','9H8FKhPVYLTX',
          'mWpKWRZZLu3R','3jBts3KWOAAP','PrgLiFypVLYw','eaZEEY2i4YT2','NwgMJByVeFyC',
          'Ht8ZUwhpA1DH','g0OYZmaGK1v8','v14qii09xWVc','z2cMreTyvWVz','6OOBk6hUKjLW']
bad = []
for nid in leaves:
    row = cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    if not row:
        bad.append((nid, '缺失'))
        continue
    c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[2],)).fetchone()[0]
    is_html = bool(re.search(r'<p|<h[1-6]|<ul|<ol|<table', c))
    cn = len(re.findall(r'[\u4e00-\u9fff]', c))
    txt = re.sub(r'<[^>]+>', ' ', c)
    en = len(re.findall(r'[A-Za-z]{3,}', txt))
    # 检查是否还有英文句子残留（排除代码块/链接文本）
    en_before_cn = 0
    for m in re.finditer(r'<p[^>]*>(.*?)</p>', c, re.S):
        p = re.sub(r'<[^>]+>', ' ', m.group(1))
        fc = re.search(r'[\u4e00-\u9fff]', p)
        if fc:
            before = p[:fc.start()]
            en_before_cn = max(en_before_cn, len(re.findall(r'[A-Za-z]{3,}', before)))
    flag = 'OK' if (is_html and cn > 50) else '!!!'
    print('%s | %s | type=%s | html=%s | cn=%d | en=%d | p前英文=%d %s' % (nid, (row[1] or '')[:40], row[2], is_html, cn, en, en_before_cn, flag))
conn.close()
