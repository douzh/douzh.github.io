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

# check links in a couple of notes
mp=json.load(open('help_map.json',encoding='utf-8'))['node_map']
# find notes that contain reference links in source
import random
hc=json.load(open('help_content.json',encoding='utf-8'))
withlinks=[k for k,v in hc.items() if 'reference-link' in v or '#root/' in v]
print('source pages with links:', len(withlinks))
for src in withlinks[:3]:
    nid=mp.get(src)
    res=call('get_note_content',{'noteId':nid})
    d=json.loads(res['result']['content'][0]['text'])
    c=d.get('content','')
    # find link snippets
    print('=== ', src, '->', nid)
    for m in re.finditer(r'<a class="reference-link" href="#root/([^"]+)">([^<]*)</a>', c):
        print('  LINK href=#root/%s text=%r' % (m.group(1), m.group(2)))
    print('  (total len', len(c), ')')
