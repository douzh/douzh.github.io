# -*- coding: utf-8 -*-
import sqlite3, json, re, os

db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 1. docName map for _help notes
rows=cur.execute("SELECT noteId, value FROM attributes WHERE isDeleted=0 AND type='label' AND name='docName' AND noteId LIKE '_help%'").fetchall()
docnames={nid:val for nid,val in rows}

# 2. _help note titles
rows=cur.execute("SELECT noteId, title FROM notes WHERE isDeleted=0 AND noteId LIKE '_help%' AND type='doc'").fetchall()
help_titles={nid:title for nid,title in rows}

# 3. bilingual notes with images
bil=json.load(open('_bil_imgs.json',encoding='utf-8'))

# map: help title (normalized) -> docName
title_to_doc={}
for nid,t in help_titles.items():
    if nid in docnames:
        title_to_doc[t]=docnames[nid]

# also map by noteId? bilingual notes may have parent linking to _help. Just use title matching.
# For each bilingual note, find matching _help by title (strip the (中文) part)
mapping=[]  # (bil_noteId, bil_title, help_docName, imgs)
unmatched=[]
for b in bil:
    title=b['title']
    # strip " (中文)" -> base english title
    base=re.sub(r'\s*\(.*?\)\s*$','',title)
    # docName uses same base path; try direct match
    docname=title_to_doc.get(base)
    if docname:
        mapping.append({'noteId':b['noteId'],'title':b['title'],'base':base,'docName':docname,'imgs':b['imgs']})
    else:
        # try matching docName ends with base
        cand=None
        for d in title_to_doc.values():
            if d.split('/')[-1]==base or d.replace('User Guide/User Guide/','').split('/')[-1]==base:
                cand=d; break
        if cand:
            mapping.append({'noteId':b['noteId'],'title':b['title'],'base':base,'docName':cand,'imgs':b['imgs']})
        else:
            unmatched.append(b)

json.dump(mapping, open('_bil_mapping.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('mapped:', len(mapping))
print('unmatched:', len(unmatched))
for u in unmatched[:30]:
    print('  UNMATCHED:', u['noteId'], u['title'], u['imgs'])
print()
print('sample mapping:')
for m in mapping[:5]:
    print('  ', m['noteId'], m['title'], '->', m['docName'], m['imgs'][:2])
