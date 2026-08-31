# -*- coding: utf-8 -*-
import json, re
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
def find(node,p='',out=None):
    if out is None: out=[]
    if isinstance(node,dict):
        if 'files' in node and isinstance(node['files'],dict):
            for n,s in node['files'].items():
                find(s,p+'/'+n,out)
        elif 'offset' in node:
            out.append((p,int(node['offset']),node.get('size')))
    return out
files=find(h['files'])
print('total files:', len(files))
mc=[f for f in files if f[0].endswith('/main.cjs')]
print('main.cjs:', mc)
