# -*- coding: utf-8 -*-
import sqlite3, json, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

def get_attrs(nid):
    rows = cur.execute("SELECT type, name, value FROM attributes WHERE noteId=?", (nid,)).fetchall()
    return rows

root = 'yTRmjAPEV7Wm'
def subtree(note_id):
    rows = cur.execute("""
      SELECT b.noteId, n.title, n.type, n.blobId
      FROM branches b JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId = ? ORDER BY b.notePosition, b.noteId
    """, (note_id,)).fetchall()
    out = []
    for r in rows:
        out.append(r)
        out += subtree(r[0])
    return out

nodes = subtree(root)
print('=== book 类型笔记详情 (%d) ===' % len([n for n in nodes if n[2]=='book']))
for nid, title, typ, blobId in nodes:
    if typ != 'book':
        continue
    content = get_blob(blobId) or ''
    attrs = get_attrs(nid)
    docpath = [a for a in attrs if a[1]=='docPath']
    has_cn = bool(re.search(r'[\u4e00-\u9fff]', content))
    is_html = bool(re.search(r'<p|<div|<h[1-6]|<ul|<table', content))
    print('%-14s | %-38s | len=%-6d cn=%s html=%s docPath=%s' % (nid, (title or '')[:38], len(content), has_cn, is_html, docpath))

print()
print('=== text 类型但内容不是HTML且含中文 (%d) ===' % len([n for n in nodes if n[2]=='text' and get_blob(n[3]) and not re.search(r'<p|<div|<h[1-6]|<ul|<table', get_blob(n[3]) or '') and re.search(r'[\u4e00-\u9fff]', get_blob(n[3]) or '')]))
for nid, title, typ, blobId in nodes:
    if typ != 'text':
        continue
    content = get_blob(blobId) or ''
    if content and not re.search(r'<p|<div|<h[1-6]|<ul|<table', content) and re.search(r'[\u4e00-\u9fff]', content):
        print('  %s | %s | len=%d' % (nid, (title or '')[:40], len(content)))
conn.close()
