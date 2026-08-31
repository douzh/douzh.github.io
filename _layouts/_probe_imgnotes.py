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

# search for image type notes
res=call('search_notes',{'query':'note.type = image','limit':20})
d=json.loads(res['result']['content'][0]['text'])
print('image notes:', d.get('totalResults'))
for r_ in d.get('results',[]):
    print('  ', r_.get('noteId'), r_.get('title'))

# search for notes titled with _image
res=call('search_notes',{'query':'_image','limit':20})
d=json.loads(res['result']['content'][0]['text'])
print('notes matching _image:', d.get('totalResults'))
for r_ in d.get('results',[])[:10]:
    print('  ', r_.get('noteId'), r_.get('title'), r_.get('parentTitle'))
