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

doc=walk(h['files'].get('assets',{}).get('files',{}).get('doc_notes',{}), '/assets/doc_notes')
tray=[f for f in doc if f[0].endswith('Tray icon & automatic startup.html')][0]
print('tray html:', tray)
blob=data[data_start+tray[1]:data_start+tray[1]+tray[2]]
t=blob.decode('utf-8','replace')
print('--- tray html full ---')
print(t)
