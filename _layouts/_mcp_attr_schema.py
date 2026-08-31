# -*- coding: utf-8 -*-
import json, urllib.request

URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
payload={'jsonrpc':'2.0','id':1,'method':'tools/list','params':{}}
req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
with urllib.request.urlopen(req,timeout=120) as r:
    data=r.read().decode('utf-8')
for line in data.splitlines():
    if line.startswith('data:'):
        obj=json.loads(line[5:].strip())
        tools=obj.get('result',{}).get('tools',[])
        for t in tools:
            if t['name'] in ('set_attribute','get_attribute','get_attributes'):
                print('===', t['name'], '===')
                print(json.dumps(t['inputSchema'], ensure_ascii=False, indent=1))
                print()
