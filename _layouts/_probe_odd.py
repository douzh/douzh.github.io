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

# check specific notes with odd links
targets={'hcZlVY6xuwTM':'Backlinks','7GvIUovXTWfm':'Note Map','jEjptFSIqsK5':'Printing'}
for nid,title in targets.items():
    res=call('get_note_content',{'noteId':nid})
    d=json.loads(res['result']['content'][0]['text'])
    c=d.get('content','')
    print('===', title)
    # find all href with _help anywhere
    for m in re.finditer(r'href="#root/[^"]*_help[^"]*"', c):
        s=max(0,m.start()-10); e=min(len(c),m.end()+10)
        print('  ', repr(c[s:e]))
    for m in re.finditer(r'\(#root/[^)]*_help[^)]*\)', c):
        s=max(0,m.start()-10); e=min(len(c),m.end()+10)
        print('  MD', repr(c[s:e]))
