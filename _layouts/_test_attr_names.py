# -*- coding: utf-8 -*-
import json, urllib.request

URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def raw(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    try:
        with urllib.request.urlopen(req,timeout=120) as r:
            data=r.read().decode('utf-8')
    except Exception as e:
        return 'ERR:'+str(e)
    out=[]
    for line in data.splitlines():
        line=line.strip()
        if line.startswith('data:'):
            try: out.append(json.loads(line[5:].strip()))
            except: pass
    if out:
        return json.dumps(out[-1], ensure_ascii=False)
    return 'NOOUT'

# test various attribute names to understand the dangerous list
tests=[
    ('docName','User Guide/User Guide/Installation & Setup'),
    ('docPath','User Guide/User Guide/Installation & Setup'),
    ('helpDocName','User Guide/User Guide/Installation & Setup'),
    ('customResourceProvider','/tmp/x'),
    ('run','frontendStartup'),
    ('widget','x'),
    ('myTestAttr','hello'),
    ('template','x'),
    ('iconClass','bx bx-folder'),
]
for name,val in tests:
    r=raw('set_attribute', {'noteId':'3GTPwnM0LXzq','type':'label','name':name,'value':val})
    print(f'{name:25s} -> {r}')
