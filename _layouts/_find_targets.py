# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
# 在整个库中按标题查
for title in ['API Reference','grafana-dashboard.json','Import & Export','导入与导出','ETAPI','Metrics','指标']:
    rows = cur.execute("SELECT noteId, title, type FROM notes WHERE title=?", (title,)).fetchall()
    print('%s => %s' % (title, rows))
conn.close()
