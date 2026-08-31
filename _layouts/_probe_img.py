# -*- coding: utf-8 -*-
import json, urllib.request, re

URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}

def call(tool,args):
    payload={'jsonrpc':'2.0','id':99,'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    with urllib.request.urlopen(req,timeout=180) as r:
        data=r.read().decode('utf-8')
    out=[]
    for line in data.splitlines():
        line=line.strip()
        if line.startswith('data:'):
            try: out.append(json.loads(line[5:].strip()))
            except: pass
    return out[-1] if out else None

def get_children(nid):
    res=call('get_child_notes',{'noteId':nid})
    d=json.loads(res['result']['content'][0]['text'])
    return d if isinstance(d,list) else d.get('results',[])

nodes=[]
def walk(nid):
    for k in get_children(nid):
        nodes.append((k['noteId'],k['title'],k.get('type')))
        if k.get('childCount',0)>0:
            walk(k['noteId'])
walk('yTRmjAPEV7Wm')

img_styles=set(); img_srcs=set(); img_notes=0; sample=[]
for nid,title,ntype in nodes:
    if ntype not in ('text','book'):
        continue
    res=call('get_note_content',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        c=d.get('content','')
    except:
        continue
    if '<img' in c:
        img_notes+=1
        for m in re.finditer(r'<img[^>]*>', c):
            tag=m.group(0)
            src=re.search(r'src="([^"]*)"', tag)
            st=re.search(r'style="([^"]*)"', tag)
            if src: img_srcs.add(src.group(1))
            if st: img_styles.add(st.group(1))
            if len(sample)<5:
                sample.append((nid,title,tag[:200]))

print('notes with <img>:', img_notes)
print('distinct src values:', len(img_srcs))
for s in sorted(img_srcs)[:40]:
    print('  SRC:', repr(s))
print()
print('distinct style values:', len(img_styles))
for s in sorted(img_styles)[:15]:
    print('  STYLE:', repr(s))
print()
print('--- samples ---')
for s in sample:
    print(s[0], s[1], ':', s[2])
