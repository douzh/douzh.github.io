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

# collect new ids from map
mp=json.load(open('help_map.json',encoding='utf-8'))['node_map']
new_ids=set(mp.values())

leftover=[]; all_links=0; deadlinks=[]
for nid,title,ntype in nodes:
    if ntype not in ('text','book'):
        continue
    res=call('get_note_content',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        c=d.get('content','')
    except:
        continue
    # 1) any leftover _help token
    for m in re.finditer(r'_help_[A-Za-z0-9]+', c):
        leftover.append((nid,title,m.group(0)))
    # 2) count links pointing to new ids
    for m in re.finditer(r'#root/([A-Za-z0-9]{10,})', c):
        all_links+=1
        if m.group(1) not in new_ids and not m.group(1).startswith('_'):
            deadlinks.append((nid,title,m.group(1)))

print('leftover _help refs:', len(leftover))
for l in leftover[:20]:
    print('  ', l)
print('total internal links:', all_links)
print('links to unknown (non-_help, non-new) ids:', len(deadlinks))
for d_ in deadlinks[:20]:
    print('  ', d_)
