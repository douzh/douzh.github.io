# -*- coding: utf-8 -*-
import json, urllib.request, time, sys

URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    try:
        with urllib.request.urlopen(req,timeout=60) as r:
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
        content=res.get('content')
        txt=''
        if isinstance(content,list):
            for c in content:
                if isinstance(c,dict): txt+=c.get('text','')
        return txt or json.dumps(res,ensure_ascii=False)
    return 'NOOUT'

plan=json.load(open('_attr_plan.json',encoding='utf-8'))
ok=fail=0
results=[]
for m in plan:
    r=call('set_attribute', {'noteId':m['noteId'],'type':'label','name':'docPath','value':m['docName']})
    if r.startswith('{"success"') or '"success":true' in r:
        ok+=1
    else:
        fail+=1
        print('FAIL', m['noteId'], m['title'], '->', r)
    results.append((m['noteId'], r))
    time.sleep(0.1)
print(f'\nDONE: ok={ok} fail={fail} total={len(plan)}')
json.dump(results, open('_attr_set_results.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
