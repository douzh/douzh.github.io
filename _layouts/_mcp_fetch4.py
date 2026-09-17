# -*- coding: utf-8 -*-
import json, urllib.request, sys
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
def call(tool, args):
    payload={'jsonrpc':'2.0','id':1,'method':'tools/call','params':{'name':tool,'arguments':args}}
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

for nid in ['mHbBMPDPkVV5','pgxEVkzLl1OP','uYF7pmepw27K','W8vYD3Q1zjCR']:
    r = call('get_note_content', {'noteId':'_help_'+nid})
    with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\_help_%s.md'%nid,'w',encoding='utf-8') as f:
        f.write(r)
    print('_help_%s len=%d' % (nid, len(r)))
