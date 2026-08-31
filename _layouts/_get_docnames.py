# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# attributes schema
print('attrs schema:', [r[1] for r in cur.execute("PRAGMA table_info(attributes)").fetchall()])
# get docName attributes for _help notes
rows=cur.execute("""
SELECT a.noteId, a.name, a.value
FROM attributes a
WHERE a.isDeleted=0 AND a.type='label' AND a.name='docName' AND a.noteId LIKE '_help%'
""").fetchall()
print('docName attrs:', len(rows))
# build map
docmap={}
for nid,name,val in rows:
    docmap[nid]=val
json.dump(docmap, open('_help_docnames.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
# sample
items=list(docmap.items())[:10]
for k,v in items:
    print('  ', k, '->', v)
