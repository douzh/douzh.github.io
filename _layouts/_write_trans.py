# -*- coding: utf-8 -*-
import json, urllib.request, time, re, sys
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    with urllib.request.urlopen(req,timeout=180) as r:
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

id_map=json.load(open('_id_map.json',encoding='utf-8'))

# 翻译批次文件: _trans_batch*.py = TRANS dict
ns={}
exec(compile(open(sys.argv[1],encoding='utf-8').read(), sys.argv[1], 'exec'), ns)
trans=ns['TRANS']
def repl(m):
    hid=m.group(1)
    return '#root/'+id_map[hid] if hid in id_map else '#root/'+hid

ok=fail=0
for nid,md in trans.items():
    md2=re.sub(r'#root/(_help_[A-Za-z0-9]+)', repl, md)
    r=call('set_note_content', {'noteId':nid,'content':md2})
    if '"success":true' in r or r.startswith('{"success'):
        ok+=1
    else:
        fail+=1
        print('FAIL', nid, r[:200])
    time.sleep(0.1)
print(f'batch={sys.argv[1]} ok={ok} fail={fail}')
