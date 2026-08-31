# -*- coding: utf-8 -*-
import json, urllib.request, sys, time

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
        # check isError
        if last.get('result',{}).get('isError'):
            return 'ERR:'+str(last.get('result',{}).get('content'))
        return 'OK'
    return 'NOOUT'

if __name__=='__main__':
    plan=json.load(open('_attr_plan.json',encoding='utf-8'))
    # test first one
    m=plan[0]
    r=call('set_attribute', {'noteId':m['noteId'],'type':'label','name':'docName','value':m['docName']})
    print('test:', m['noteId'], m['title'], '->', r)
