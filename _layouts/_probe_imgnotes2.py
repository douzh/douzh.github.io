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

# check source help content for these image notes - how are they referenced in the raw doc content?
# Search the help content for one of the image note titles
res=call('search_notes',{'query':'note.type = image','limit':100})
d=json.loads(res['result']['content'][0]['text'])
print('all image notes:', d.get('totalResults'))
# check where they live
for r_ in d.get('results',[]):
    print('  ', r_.get('noteId'), repr(r_.get('title')), 'parent:', r_.get('parentTitle'))

# check an image note metadata
res=call('get_note',{'noteId':'OmISaHjINmQ7'})
print()
print('image note meta:', json.dumps(res,ensure_ascii=False)[:400])
