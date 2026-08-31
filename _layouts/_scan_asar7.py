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
            out.append((p, int(node['offset']), node.get('size')))
    return out

assets_files=header['files']['assets']['files']
doc=walk_files(assets_files.get('doc_notes',{}), '/assets/doc_notes')
# find tray icon html
hits=[f for f in doc if 'tray-icon-and-startup' in f[0].lower() or 'Tray icon' in f[0]]
print('tray hits:', hits)
# find all html under User Guide
htmls=[f for f in doc if f[0].endswith('.html')]
print('total html under doc_notes:', len(htmls))
