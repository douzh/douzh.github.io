# -*- coding: utf-8 -*-
import json, urllib.request, time

URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
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
        last=out[-1]
        res=last.get('result',{})
        if res.get('isError'):
            return 'ERR:'+json.dumps(res.get('content'))
        content=res.get('content')
        if isinstance(content,list):
            return json.dumps([c.get('text','') for c in content], ensure_ascii=False)
        return json.dumps(content, ensure_ascii=False)
    return 'NOOUT'

r=call('get_attributes', {'noteId':'3GTPwnM0LXzq'})
print(r)
time.sleep(1)
r2=call('get_attributes', {'noteId':'3GTPwnM0LXzq'})
print('after 1s:', r2)
