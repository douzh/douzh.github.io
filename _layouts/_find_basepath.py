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
# search all public js for assetPath / isStandalone / server-assets
for f in pub:
    p,off,sz=f
    if not p.endswith('.js'): continue
    if sz<100 or sz>3000000: continue
    blob=data[data_start+off:data_start+off+sz]
    t=blob.decode('utf-8','replace')
    if ('assetPath' in t or 'server-assets' in t or 'isStandalone' in t):
        for kw in ['server-assets','assetPath','isStandalone']:
            if kw in t:
                idxs=[m.start() for m in re.finditer(re.escape(kw), t)]
                # print concise
                for i in idxs[:2]:
                    seg=t[max(0,i-120):i+140].replace(chr(10),' ')
                    if 'server-assets' in seg or 'assetPath' in seg or 'isStandalone' in seg:
                        print(f'[{p}] {kw}:', seg[:260])
                break
