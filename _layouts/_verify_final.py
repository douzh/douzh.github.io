# -*- coding: utf-8 -*-
import sqlite3, json, sys, re
DB = r'F:\trilium\trilium-data\document.db'
book_list = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_book_issues.json', encoding='utf-8'))
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

print('noteId        title                              len   hasCN  type')
print('-'*80)
ok = 0
missing = []
for item in book_list:
    nid = item['book_noteId']
    row = cur.execute("SELECT title, blobId, type FROM notes WHERE noteId=?", (nid,)).fetchone()
    if not row:
        missing.append(nid)
        continue
    title, blobId, typ = row
    content = get_blob(blobId) or ''
    has_cn = bool(re.search(r'[\u4e00-\u9fff]', content))
    if has_cn and len(content) > 100:
        ok += 1
    print('%-14s %-34s %6d  %s   %s' % (nid, (title or '')[:34], len(content), 'Y' if has_cn else 'N', typ))
conn.close()
print('-'*80)
print('OK count: %d/%d  missing: %s' % (ok, len(book_list), missing))
