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

mp=json.load(open('help_map.json',encoding='utf-8'))['node_map']

# collect all leftover tokens in actual content
leftover=set()
for nid,title,ntype in nodes:
    if ntype not in ('text','book'):
        continue
    res=call('get_note_content',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        c=d.get('content','')
    except:
        continue
    for m in re.finditer(r'_help_[A-Za-z0-9]+', c):
        t=m.group(0)
        leftover.add(t)

print('distinct leftover tokens:', len(leftover))
print()
# for each leftover, check if in mp, or if a case-insensitive/fuzzy match exists in mp keys
keys=list(mp.keys())
def find_candidates(tok):
    if tok in mp:
        return 'EXACT', [tok]
    # strip: some tokens may have merged/space issues
    # case-insensitive
    ci=[k for k in keys if k.lower()==tok.lower()]
    if len(ci)==1:
        return 'CI', ci
    if len(ci)>1:
        return 'CI_MULTI', ci
    # fuzzy: same length, compare
    return 'NONE', []

for tok in sorted(leftover):
    kind,cands=find_candidates(tok)
    print('%-24s %-10s %s' % (tok, kind, cands if kind!='EXACT' else mp[tok]))
