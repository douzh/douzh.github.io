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

# walk user guide
nodes=[]
def walk(nid):
    for k in get_children(nid):
        nodes.append((k['noteId'],k['title'],k.get('type')))
        if k.get('childCount',0)>0:
            walk(k['noteId'])
walk('yTRmjAPEV7Wm')
print('total nodes:', len(nodes))

mp=json.load(open('help_map.json',encoding='utf-8'))['node_map']
# build reverse: new id set
new_ids=set(mp.values())
print('new ids in map:', len(new_ids))

# regex to replace href="#root/_help_XXX" -> "#root/<new>"
pat=re.compile(r'#root/(_help_[A-Za-z0-9]+)')

updated=0; skipped=0; errs=[]
for nid,title,ntype in nodes:
    if ntype not in ('text','book'):
        continue
    res=call('get_note_content',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        c=d.get('content','')
    except Exception as e:
        errs.append((nid,title,'get:'+str(res)[:100]))
        continue
    if not c.strip():
        skipped+=1
        continue
    def rep(m):
        src=m.group(1)
        if src in mp:
            return '#root/'+mp[src]
        return m.group(0)
    newc=pat.sub(rep,c)
    if newc!=c:
        r2=call('set_note_content',{'noteId':nid,'content':newc})
        try:
            dd=json.loads(r2['result']['content'][0]['text'])
            if dd.get('success'):
                updated+=1
            else:
                errs.append((nid,title,'set:'+str(r2)[:100]))
        except Exception as e:
            errs.append((nid,title,'set:'+str(r2)[:100]))

print('updated:', updated)
print('skipped(empty):', skipped)
print('errors:', len(errs))
for e in errs[:20]:
    print('  ', e)
