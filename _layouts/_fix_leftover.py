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

# Manual leftover -> new id mapping (based on link text reverse lookup)
FIX = {
    '_help_wy8So3yZZl9':      'G6uh5oxvadSV',   # Reporting issues
    '_help_8QqMnzx393bx':     'OJRjVCOEAzaT',   # Grid View
    '_help_iRwzGnHPzomm':     'Tt58b3ruaARW',   # Relation Map
    '_help_MKmMg5x6xkor':     'nyo1rLbxdtdK',   # Archived Notes
    '_help_RnaPd bciOfeq':    'R3myDXkvEDLx',   # Right Sidebar (space variant)
    '_help_OR8W7Iz9K4U':      'A8M0RjeMsL1L',   # Note Hoisting
    '_help_AlkDUqhENtH7':     '4ACP3eqPx2dd',   # Custom app-wide CSS
    '_help_NRnIZmSMc5si':     'jEjptFSIqsK5',   # Printing & Exporting as PDF
    '_help_xmIYSP6wE3F':      '9tK6XcPShKXh',   # Launch Bar
    '_help_fiHicjpJjIRJ':     'TNBOr62S7Osb',   # Security
    '_help_8YBEPzcpUgwx':     'Y4XIsb451ZaD',   # Note buttons
    '_help_AlJ73vBCjWDr':     'NTVYMnkwiZCF',   # Status bar
    '_help_SpirpZyephBG':     'ZlacHrx96iIP',   # Backend scripts
    '_help_swSFitWk6KkA':     'KENrlyFaUbZN',   # Network Access
    '_help_GtWFsgaA0lCt':     'iLOq7QKFCwlZ',   # Collections
    '_help_KrLZFoIH0j6p':     '36GZKqQAoCXM',   # Connections tab
    '_help_ZlN4numP6EbW':     'fwhBAyFO5Puc',   # Slash Commands
}

updated=0; total_links=0; missing=[]
for nid,title,ntype in nodes:
    if ntype not in ('text','book'):
        continue
    res=call('get_note_content',{'noteId':nid})
    try:
        d=json.loads(res['result']['content'][0]['text'])
        c=d.get('content','')
    except:
        continue
    if not c.strip():
        continue
    orig=c
    for tok,newid in FIX.items():
        pat=re.compile(r'#root/'+re.escape(tok))
        c,n=pat.subn('#root/'+newid, c)
        total_links+=n
    if c!=orig:
        r2=call('set_note_content',{'noteId':nid,'content':c})
        try:
            dd=json.loads(r2['result']['content'][0]['text'])
            if dd.get('success'):
                updated+=1
            else:
                missing.append((nid,title,'set:'+str(r2)[:100]))
        except:
            missing.append((nid,title,'set:'+str(r2)[:100]))

print('notes updated:', updated)
print('total leftover links fixed:', total_links)
print('errors:', len(missing))
for m in missing[:10]:
    print('  ', m)
