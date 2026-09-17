# -*- coding: utf-8 -*-
import sqlite3, json, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
for nid in ['BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM','RC8UiNCM0fHL']:
    row = cur.execute("SELECT noteId, title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    content = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[3],)).fetchone()[0]
    is_html = bool(re.search(r'<p|<h[1-6]|<ul|<table', content))
    cn = len(re.findall(r'[\u4e00-\u9fff]', content))
    print('%s | %s | type=%s | len=%d | html=%s | cn=%d' % (nid, row[1], row[2], len(content), is_html, cn))
    print('  head:', content[:100].replace(chr(10),' '))
conn.close()
