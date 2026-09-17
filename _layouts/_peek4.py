# -*- coding: utf-8 -*-
import sqlite3, json, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

for nid in ['BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM','RC8UiNCM0fHL']:
    row = cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    content = get_blob(row[2]) or ''
    print('='*100)
    print('%s | %s | %s | len=%d' % (nid, row[0], row[1], len(content)))
    print(content[:2500])
    print()
conn.close()
