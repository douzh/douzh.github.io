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
    return data

print('=== get_attributes raw ===')
print(raw('get_attributes', {'noteId':'3GTPwnM0LXzq'}))
print()
print('=== set_attribute raw ===')
print(raw('set_attribute', {'noteId':'3GTPwnM0LXzq','type':'label','name':'docName','value':'User Guide/User Guide/Installation & Setup/Desktop Installation/Tray icon & automatic startup'}))
