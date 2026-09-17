# -*- coding: utf-8 -*-
import sqlite3
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()
for nid in ['_help_nRqcgfTb97uV','_help_Rp0q8bSP6Ayl','_help_hvRfsDr591WP']:
    row = cur.execute("SELECT * FROM notes WHERE noteId=?", (nid,)).fetchone()
    cols = [d[0] for d in cur.description]
    print('=== %s ===' % nid)
    for c, v in zip(cols, row):
        print('  %s = %r' % (c, str(v)[:120]))
    # 查 revisions
    revs = cur.execute("SELECT * FROM revisions WHERE noteId=?", (nid,)).fetchall()
    print('  revisions:', len(revs))
conn.close()
