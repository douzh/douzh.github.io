# -*- coding: utf-8 -*-
import json, re
header=json.load(open('_asar_header.json',encoding='utf-8'))

def walk_files(node, p='', out=None):
    if out is None: out=[]
    if isinstance(node, dict):
        if 'files' in node and isinstance(node['files'], dict):
            for name, sub in node['files'].items():
                walk_files(sub, p+'/'+name, out)
        elif 'offset' in node:
            out.append((p, node.get('size')))
    return out

assets_files=header['files']['assets']['files']
print('assets.files keys:', list(assets_files.keys()))

doc=walk_files(assets_files.get('doc_notes',{}), '/assets/doc_notes')
print('\ndoc_notes total:', len(doc))
for f in doc[:80]:
    print('  ', f)
