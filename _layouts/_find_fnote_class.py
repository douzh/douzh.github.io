# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
def walk(node,p='',out=None):
    if out is None: out=[]
    if isinstance(node,dict):
        if 'files' in node and isinstance(node['files'],dict):
            for n,s in node['files'].items():
                walk(s,p+'/'+n,out)
        elif 'offset' in node:
            out.append((p,int(node['offset']),node.get('size')))
    return out
pub=walk(h['files'].get('public',{}), '/public')
# find FNote class - getLabelValue and getAttributes
for f in pub:
    p,off,sz=f
    if not p.endswith('.js'): continue
    if sz<500 or sz>3000000: continue
    blob=data[data_start+off:data_start+off+sz]
    t=blob.decode('utf-8','replace')
    if 'getLabelValue(' in t and 'getAttribute(' in t and ('getOwnedAttributes' in t or 'getAttributes(' in t):
        # this could be the note entity class
        i=t.find('getLabelValue(')
        seg=t[max(0,i-500):i+800]
        # print around getLabelValue
        j=t.rfind('getLabelValue(', 0, i+1)
        print(f'[{p}]')
        print(seg[:1200])
        break
