# -*- coding: utf-8 -*-
import json, urllib.request
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
payload={'jsonrpc':'2.0','id':1,'method':'tools/list','params':{}}
req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
with urllib.request.urlopen(req,timeout=60) as r:
    data=r.read().decode('utf-8')
out=[]
for line in data.splitlines():
    line=line.strip()
    if line.startswith('data:'):
        try: out.append(json.loads(line[5:].strip()))
        except: pass
res=out[-1].get('result',{})
tools=res.get('tools',[])
for t in tools:
    if t['name'] in ('set_note_content','edit_note_content','append_to_note'):
        print(t['name'], '->', json.dumps(t.get('inputSchema',{}), ensure_ascii=False))
