# -*- coding: utf-8 -*-
import sqlite3, json, re, struct

# ---- load asar file list ----
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
# relative path -> size
asar_files={}
for p,off,sz in doc:
    if p.startswith('/assets/doc_notes/'):
        rel=p[len('/assets/doc_notes/'):]  # en/User Guide/...
        asar_files[rel]=sz

print('asar doc_notes files:', len(asar_files))

# ---- load mapping ----
mapping=json.load(open('_bil_mapping.json',encoding='utf-8'))

PREFIX='assets/v0.105.0/doc_notes/'
report=[]
missing=[]
fixed_count=0
for m in mapping:
    docname=m['docName']  # e.g. User Guide/User Guide/Installation & Setup/Tray icon...
    # directory = en/<dirname>
    docdir='en/'+docname
    # docName points to html file: en/<docname>.html; image dir = en/<dirname of html>
    dirpart=docdir[:docdir.rfind('/')]
    updates=[]
    ok=True
    for src in m['imgs']:
        fname=src
        # decode url-encoding
        import urllib.parse
        fname_dec=urllib.parse.unquote(fname)
        fname_html=fname.replace(' ','%20').replace('&','%26')
        # candidate paths
        cand1=dirpart+'/'+fname_html
        cand2=dirpart+'/'+urllib.parse.quote(fname)  # fully encoded
        cand_dec=dirpart+'/'+fname_dec
        if cand1 in asar_files:
            newurl=PREFIX+urllib.parse.quote(cand1)
            updates.append((src,newurl))
        elif cand_dec in asar_files:
            newurl=PREFIX+urllib.parse.quote(cand_dec)
            updates.append((src,newurl))
        else:
            # check other dirs containing this filename (fallback)
            matches=[k for k in asar_files if k.split('/')[-1]==fname_dec]
            if matches:
                newurl=PREFIX+urllib.parse.quote(matches[0])
                updates.append((src,newurl))
            else:
                ok=False
                report.append(('MISSING_FILE', m['noteId'], m['title'], docname, src))
    if ok and updates:
        fixed_count+=1
        report.append(('OK', m['noteId'], m['title'], docname, [u[0] for u in updates]))

json.dump(report, open('_fix_report.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('notes fully fixable:', fixed_count)
print('total report entries:', len(report))
print()
miss=[r for r in report if r[0]=='MISSING_FILE']
print('missing file issues:', len(miss))
for r in miss:
    print('  ', r[1], r[2], '|', r[3], '|', r[4])
