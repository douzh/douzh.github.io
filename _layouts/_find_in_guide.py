# -*- coding: utf-8 -*-
import sqlite3
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
root = 'yTRmjAPEV7Wm'
def subtree(note_id):
    rows = cur.execute("""
      SELECT b.noteId, n.title, n.type
      FROM branches b JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId = ? ORDER BY b.notePosition, b.noteId
    """, (note_id,)).fetchall()
    out = []
    for r in rows:
        out.append(r)
        out += subtree(r[0])
    return out
nodes = subtree(root)
print('总节点:', len(nodes))
for nid, title, typ in nodes:
    t = title or ''
    if 'API Reference' in t or 'grafana' in t.lower() or 'dashboard' in t.lower():
        print('%s | %s | %s' % (nid, t, typ))
conn.close()
