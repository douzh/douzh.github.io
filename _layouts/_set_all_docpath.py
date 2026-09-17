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
        content=res.get('content')
        txt=''
        if isinstance(content,list):
            for c in content:
                if isinstance(c,dict): txt+=c.get('text','')
        return txt or json.dumps(res,ensure_ascii=False)
    return 'NOOUT'

mapping=json.load(open('_manual_mapping.json',encoding='utf-8'))
# 排除已标项 Bgf8cWUPyF9q
exclude={'Bgf8cWUPyF9q'}
todo={k:v for k,v in mapping.items() if k not in exclude}
print('待打标笔记数:', len(todo))

ok=fail=0
fails=[]
for nid,dn in todo.items():
    r=call('set_attribute', {'noteId':nid,'type':'label','name':'docPath','value':dn})
    if '"success":true' in r or r.startswith('{"success"'):
        ok+=1
    else:
        fail+=1
        fails.append((nid,dn,r))
        print('FAIL', nid, dn, '->', r)
    time.sleep(0.08)

print(f'\n完成: ok={ok} fail={fail} total={len(todo)}')
if fails:
    json.dump(fails, open('_attr_set_fails.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
