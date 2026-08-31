# -*- coding: utf-8 -*-
import json, struct, re, urllib.parse
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
doc=walk(h['files'].get('assets',{}).get('files',{}).get('doc_notes',{}), '/assets/doc_notes')
def read(docname):
    rel='en/'+docname+'.html'
    for p,off,sz in doc:
        if p[len('/assets/doc_notes/'):]==rel:
            return data[data_start+off:data_start+off+sz].decode('utf-8','replace')
    return None

t=read('User Guide/User Guide/Advanced Usage/ETAPI (REST API)')  # placeholder
# find frontend API doc
t2=read('User Guide/User Guide/Scripting/Frontend Basics')
# look for frontend API note
for name in ['Frontend API','Backend API']:
    # find docName
    pass

# Search doc_notes for Frontend API html
for p,off,sz in doc:
    if 'Frontend API' in p and p.endswith('.html'):
        print('FOUND:', p)
