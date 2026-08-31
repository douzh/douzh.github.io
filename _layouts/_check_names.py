# -*- coding: utf-8 -*-
import sqlite3, json, re, struct, html

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
asar_files={}
for p,off,sz in doc:
    if p.startswith('/assets/doc_notes/'):
        rel=p[len('/assets/doc_notes/'):]
        asar_files[rel]=sz

# check tray icon file name in asar
print('files containing Tray icon:')
for k in asar_files:
    if 'Tray icon' in k:
        print('  ', repr(k))
print()
print('files containing Printing & Exporting:')
for k in asar_files:
    if 'Printing & Exporting' in k:
        print('  ', repr(k))
print()
print('files containing Audio & Video:')
for k in asar_files:
    if 'Audio & Video' in k:
        print('  ', repr(k))
