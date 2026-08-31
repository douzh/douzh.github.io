# -*- coding: utf-8 -*-
import json
header=json.load(open('_asar_header.json',encoding='utf-8'))

def walk_files(node, p='', out=None):
    if out is None: out=[]
    if isinstance(node, dict):
        if 'files' in node:
            for name, sub in node['files'].items():
                walk_files(sub, p+'/'+name, out)
        elif 'offset' in node:
            out.append((p, node.get('size')))
    return out

img=walk_files(header['files'].get('assets',{}).get('files',{}).get('images',{}), '/assets/images')
print('images total:', len(img))
for f in img[:60]:
    print(f)
print()
doc=walk_files(header['files'].get('assets',{}).get('files',{}).get('db',{}), '/assets/db')
print('db assets total:', len(doc))
for f in doc[:40]:
    print(f)
