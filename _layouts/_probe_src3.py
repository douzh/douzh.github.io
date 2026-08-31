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

# Try reading source help content
res=call('get_note_content',{'noteId':'7iwde4lsKc6O'})
print('get_note_content 7iwde4lsKc6O:', json.dumps(res,ensure_ascii=False)[:300])
print()

# try search_help to find image references
res=call('search_help',{'query':'image','limit':5})
print('search_help:', json.dumps(res,ensure_ascii=False)[:500])
print()

# List root children to understand structure
res=call('get_child_notes',{'noteId':'root'})
d=json.loads(res['result']['content'][0]['text'])
print('root children:')
for k in (d if isinstance(d,list) else d.get('results',[])):
    print('  ', k.get('noteId'), k.get('title'), 'count:', k.get('childCount'))
