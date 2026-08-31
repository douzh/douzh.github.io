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

# search frontend files for runOnNoteLoad / onNoteLoad / content rendered events
pub=walk(h['files'].get('public',{}), '/public')
for f in pub:
    p,off,sz=f
    if not p.endswith('.js'): continue
    if sz<200 or sz>3000000: continue
    blob=data[data_start+off:data_start+off+sz]
    t=blob.decode('utf-8','replace')
    for kw in ['runOnNoteLoad','onNoteLoad','noteLoaded','activeNoteChanged','contentRendered','contentLoaded','noteContext']:
        if kw in t:
            idxs=[m.start() for m in re.finditer(re.escape(kw), t)]
            for i in idxs[:2]:
                seg=t[max(0,i-100):i+200].replace(chr(10),' ')
                print(f'[{p}] {kw}:', seg[:300])
            break
