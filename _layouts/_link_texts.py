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

def get_children(nid):
    res=call('get_child_notes',{'noteId':nid})
    d=json.loads(res['result']['content'][0]['text'])
    return d if isinstance(d,list) else d.get('results',[])

nodes=[]
def walk(nid):
    for k in get_children(nid):
        nodes.append((k['noteId'],k['title'],k.get('type')))
        if k.get('childCount',0)>0:
            walk(k['noteId'])
walk('yTRmjAPEV7Wm')

# tokens that were not mapped
toks=['_help_8QqMnzx393bx','_help_8YBEPzcpUgwx','_help_AlJ73vBCjWDr','_help_AlkDUqhENtH7','_help_MKmMg5x6xkor','_help_NRnIZmSMc5si','_help_OR8W7Iz9K4U','_help_SpirpZyephBG','_help_fiHicjpJjIRJ','_help_iRwzGnHPzomm','_help_iRwzGnHPzomn','_help_swSFitWk6KkA','_help_wy8So3yZZl9','_help_xmIYSP6wE3F','_help_RnaPd']

print('--- link texts for leftover tokens ---')
for nid,title,ntype in nodes:
    if ntype not in ('text','book'):
        continue
    res=call('get_note_content',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        c=d.get('content','')
    except:
        continue
    for t in toks:
        # find <a ... href="#root/TOKEN...">text</a> or [text](#root/TOKEN)
        for m in re.finditer(r'<a class="reference-link" href="#root/'+re.escape(t)+r'[^"]*"[^>]*>([^<]*)</a>', c):
            print('HTML', t, 'in', title, '->', m.group(1))
        for m in re.finditer(r'\[([^\]]+)\]\(#root/'+re.escape(t)+r'[^)]*\)', c):
            print('MD  ', t, 'in', title, '->', m.group(1))
