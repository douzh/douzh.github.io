# -*- coding: utf-8 -*-
import json, urllib.request
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
# 读取 Note Types 的 _help 节点渲染内容
r=call('get_note_content', {'noteId':'_help_KSZ04uQ2D1St'})
print('len:', len(r))
print(r[:500])
