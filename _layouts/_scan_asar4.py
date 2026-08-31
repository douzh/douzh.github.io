# -*- coding: utf-8 -*-
import json, re
header=json.load(open('_asar_header.json',encoding='utf-8'))

# Correct walk: node with 'files' dict; leaf has 'offset'
def walk_files(node, p='', out=None):
    if out is None: out=[]
    if isinstance(node, dict):
        if 'files' in node and isinstance(node['files'], dict):
            for name, sub in node['files'].items():
                walk_files(sub, p+'/'+name, out)
        elif 'offset' in node:
            out.append((p, node.get('size')))
    return out

allf=walk_files(header['files'])
print('total files:', len(allf))

# doc_notes
doc=walk_files(header['files'].get('assets',{}).get('files',{}).get('db',{}).get('files',{}).get('doc_notes',{}), '/assets/db/doc_notes')
print('\ndoc_notes total:', len(doc))
for f in doc[:60]:
    print('  ', f)

# all images anywhere
print('\nALL images:')
imgs=[f for f in allf if re.search(r'\.(png|webp|jpg|jpeg|gif|avif)$', f[0], re.I)]
print('count:', len(imgs))
for f in imgs:
    print('  ', f)
