# -*- coding: utf-8 -*-
"""查 _help_ 前缀节点，按标题匹配 25 个叶子；同时打印叶子全部属性"""
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

# 1) 打印叶子全部属性
print('=== 叶子属性 ===')
for nid in leaves:
    attrs = cur.execute("SELECT name, value, type FROM attributes WHERE noteId=? AND isDeleted=0", (nid,)).fetchall()
    print('%s: %s' % (nid, attrs))

# 2) 列出所有 _help_ 前缀节点
print('\n=== _help_ 节点 ===')
helps = cur.execute("SELECT noteId, title, type, blobId FROM notes WHERE noteId LIKE '_help_%' AND isDeleted=0 ORDER BY title").fetchall()
print('共 %d 个' % len(helps))
for h in helps:
    print('%s | %s | %s' % (h[0], (h[1] or '')[:60], h[2]))

# 3) 尝试按标题匹配
print('\n=== 标题匹配 ===')
leaf_titles = {}
for nid in leaves:
    t = cur.execute("SELECT title FROM notes WHERE noteId=?", (nid,)).fetchone()[0]
    leaf_titles[nid] = t
for nid, t in leaf_titles.items():
    base = t.split('(')[0].strip()
    hit = [h for h in helps if (h[1] or '').strip().lower() == base.lower()]
    if hit:
        print('%s (%s) -> %s' % (nid, t[:40], hit[0][0]))
    else:
        print('%s (%s) -> 未命中' % (nid, t[:40]))
conn.close()
