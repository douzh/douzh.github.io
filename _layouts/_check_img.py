# -*- coding: utf-8 -*-
import json, re
data=json.load(open('help_content.json',encoding='utf-8'))
# find all img tags across content
imgs=[]
for k,v in data.items():
    if isinstance(v,str) and '<img' in v:
        for m in re.finditer(r'<img[^>]*src="([^"]+)"[^>]*>', v):
            imgs.append((k, m.group(0)[:160]))
print('total img occurrences:', len(imgs))
print()
print('sample img tags:')
for x in imgs[:10]:
    print(' ', x[0], '->', x[1])
