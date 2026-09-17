# -*- coding: utf-8 -*-
import sqlite3
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
cur = conn.cursor()
for nid in ['BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM','RC8UiNCM0fHL']:
    rows = cur.execute("SELECT noteId, title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchall()
    for r in rows:
        content = cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[3],)).fetchone()
        print(r[0], '|', r[1], '|', r[2], '| len=', len(content[0]) if content else 0, '| 前60:', (content[0][:60] if content else '').replace(chr(10),' '))
conn.close()
