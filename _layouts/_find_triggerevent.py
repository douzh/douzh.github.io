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
for f in pub:
    if f[0]=='/public/src/content_renderer-BMc4Ylfn.js':
        blob=data[data_start+f[1]:data_start+f[1]+f[2]]
        t=blob.decode('utf-8','replace')
        # find triggerEvent definition
        for kw in ['triggerEvent(e,t)','triggerEvent=','function triggerEvent','triggerEvent(','this.triggerEvent']:
            idxs=[m.start() for m in re.finditer(re.escape(kw), t)]
            if idxs:
                i=idxs[0]
                print('==',kw,'==')
                print(t[max(0,i-200):i+300].replace(chr(10),' ')[:500])
                print()
        break
