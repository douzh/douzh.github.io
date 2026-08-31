# -*- coding: utf-8 -*-
import json, urllib.request, re

URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}

def call(tool,args):
    payload={'jsonrpc':'2.0','id':99,'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    with urllib.request.urlopen(req,timeout=180) as r:
        data=r.read().decode('utf-8')
    out=[]
    for line in data.splitlines():
        line=line.strip()
        if line.startswith('data:'):
            try: out.append(json.loads(line[5:].strip()))
            except: pass
    return out[-1] if out else None

# check the unknown ids
for nid in ['Hb2E70L7HPuf','WWRGzqHUFrln','LhtnZxtVsUMp','_help_iRwzGnHPzomn']:
    res=call('get_note',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        print(nid, '->', d.get('title'), '| type:', d.get('type'), '| parent:', d.get('parentNoteIds'))
    except Exception as e:
        print(nid, 'ERR', str(res)[:150])

# look at context in the notes
for nid,label in [('Bgf8cWUPyF9q','Patterns'),('zFJQZNoPNdYB','ContextMenu'),('jEjptFSIqsK5','Printing'),('QnI6RA5ZtkQl','Markdown')]:
    res=call('get_note_content',{'noteId':nid})
    d=json.loads(res['result']['content'][0]['text'])
    c=d.get('content','')
    print('===',label)
    for pat in ['_help_iRwzGnHPzomn','Hb2E70L7HPuf','WWRGzqHUFrln','LhtnZxtVsUMp']:
        for m in re.finditer(r'.{50}'+re.escape(pat)+r'.{50}', c):
            print('  ', pat, ':', repr(m.group(0)))
