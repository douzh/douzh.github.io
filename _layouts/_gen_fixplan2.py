# -*- coding: utf-8 -*-
import sqlite3, json, re, struct, html, urllib.parse

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
        asar_files[p[len('/assets/doc_notes/'):]]=sz

# index by basename too
basename_index={}
for k in asar_files:
    b=k.split('/')[-1]
    basename_index.setdefault(b,[]).append(k)

mapping=json.load(open('_bil_mapping.json',encoding='utf-8'))
PREFIX='assets/v0.105.0/doc_notes/'

def candidates(src):
    """return list of plausible decoded filenames for src"""
    outs=set()
    # raw
    outs.add(src)
    # html unescape
    outs.add(html.unescape(src))
    # url unquote
    outs.add(urllib.parse.unquote(src))
    # html unescape then unquote
    outs.add(urllib.parse.unquote(html.unescape(src)))
    return outs

def build_url(docname, src):
    dirpart='en/'+docname
    dirpart=dirpart[:dirpart.rfind('/')]
    for cand in candidates(src):
        if '/' not in cand:
            p=dirpart+'/'+cand
            if p in asar_files:
                return PREFIX+urllib.parse.quote(p)
    # fallback by basename
    for cand in candidates(src):
        base=cand.split('/')[-1]
        matches=basename_index.get(base,[])
        if matches:
            return PREFIX+urllib.parse.quote(matches[0])
    return None

fixplan=[]
missing=[]
for m in mapping:
    per_note=[]
    for src in m['imgs']:
        url=build_url(m['docName'], src)
        per_note.append((src,url))
    ok=all(u for _,u in per_note)
    fixplan.append({'noteId':m['noteId'],'title':m['title'],'docName':m['docName'],'imgs':per_note,'allOk':ok})
    if not ok:
        missing.append((m['noteId'],m['title'],m['docName'],[s for s,u in per_note if not u]))

json.dump(fixplan, open('_fixplan.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('notes in fixplan:', len(fixplan))
print('fully ok:', sum(1 for f in fixplan if f['allOk']))
print('has missing:', len(missing))
for nid,t,d,miss in missing:
    print('  MISS:', nid, t, '|', d, '|', miss)
