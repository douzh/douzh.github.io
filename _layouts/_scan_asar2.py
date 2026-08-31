# -*- coding: utf-8 -*-
import json, re

header=json.load(open('_asar_header.json',encoding='utf-8'))

# walk all files
allfiles=[]
def walk(node, p=''):
    if isinstance(node, dict):
        if 'files' in node:
            for name, sub in node['files'].items():
                walk(sub, p+'/'+name)
        elif 'offset' in node:
            allfiles.append((p, node.get('size')))
walk(header['files'])
print('total files:', len(allfiles))

# search for anything with image ext or help-related
for pat in [r'\.(png|webp|jpg|jpeg|gif|svg|avif)$', r'help', r'doc', r'User Guide', r'_image']:
    hits=[f for f in allfiles if re.search(pat, f[0], re.I)]
    print(f'\npattern {pat}: {len(hits)} hits')
    for h in hits[:30]:
        print('  ', h)
