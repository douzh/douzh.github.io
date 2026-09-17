# -*- coding: utf-8 -*-
import json, urllib.request, time
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    with urllib.request.urlopen(req,timeout=120) as r:
        data=r.read().decode('utf-8')
    out=[]
    for line in data.splitlines():
        line=line.strip()
        if line.startswith('data:'):
            try: out.append(json.loads(line[5:].strip()))
            except: pass
    if out:
        res=out[-1].get('result',{})
        content=res.get('content')
        txt=''
        if isinstance(content,list):
            for c in content:
                if isinstance(c,dict): txt+=c.get('text','')
        return txt
    return ''

# 给 3 个新笔记打 docPath 标签
attrs = [
    ('BM2PpcgGjM0v',  'docPath', 'User Guide/User Guide/Basic Concepts and Features/Import & Export'),
    ('x6d4zSRRm1Id',  'docPath', 'User Guide/User Guide/ETAPI (REST API)'),
    ('6HcOzchIPSTM',  'docPath', 'User Guide/User Guide/ETAPI (REST API)/Metrics'),
]
for nid, name, value in attrs:
    r = call('set_attribute', {'noteId': nid, 'type':'label', 'name': name, 'value': value})
    print(nid, name, '->', r[:150])
    time.sleep(0.2)
