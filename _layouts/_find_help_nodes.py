# -*- coding: utf-8 -*-
import sqlite3
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
rows = cur.execute("SELECT noteId, title FROM notes WHERE noteId LIKE '_help_%'").fetchall()
print('_help 节点总数:', len(rows))
for nid, title in rows:
    t = title or ''
    if any(s in t.lower() for s in ['import','export','etapi','metric','file']):
        print('%s | %s' % (nid, t))
conn.close()
