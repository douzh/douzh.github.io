# -*- coding: utf-8 -*-
import json, urllib.request

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

kids=get_children('_hidden')
print('_hidden children:', len(kids))
for k in kids:
    print('  ', k.get('noteId'), repr(k.get('title')), 'type:', k.get('type'), 'count:', k.get('childCount'))
