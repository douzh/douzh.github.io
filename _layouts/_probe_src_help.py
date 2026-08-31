# -*- coding: utf-8 -*-
import json, urllib.request

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

# Source help note for Tray icon
res=call('get_note',{'noteId':'7iwde4lsKc6O'})
try:
    d=json.loads(res['result']['content'][0]['text'])
    print('source _help_7iwde4lsKc6O:')
    print('  title:', d.get('title'))
    print('  type:', d.get('type'))
    print('  parentNoteIds:', d.get('parentNoteIds'))
    print('  attachments:', d.get('attachments',{}).get('totalCount'))
    for a in d.get('attachments',{}).get('results',[])[:10]:
        print('    ', a)
except Exception as e:
    print('ERR', str(res)[:300])

# also try getting source help note content to see how image is referenced
res=call('get_note_content',{'noteId':'7iwde4lsKc6O'})
try:
    d=json.loads(res['result']['content'][0]['text'])
    c=d.get('content','')
    import re
    for m in re.finditer(r'<img[^>]*>', c):
        print('SRC IMG:', m.group(0)[:200])
except Exception as e:
    print('CONTENT ERR', str(res)[:200])
