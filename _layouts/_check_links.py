# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
leaves = ['aIwhuIM42UXZ','KENrlyFaUbZN','3GTPwnM0LXzq','5HwfHJFwczl9','o5byGpiYwkAf','NwgMJByVeFyC','v14qii09xWVc']
for nid in leaves:
    row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
    links = re.findall(r'href="#root/([^"]+)"', c)
    help_links = [l for l in links if l.startswith('_help_')]
    guide_links = [l for l in links if not l.startswith('_help_') and not l.startswith('http')]
    print('%s | %s | 链接总数=%d _help残留=%d 用户指南=%s' % (nid, (row[0] or '')[:45], len(links), len(help_links), guide_links[:6]))
conn.close()
