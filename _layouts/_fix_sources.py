# -*- coding: utf-8 -*-
import sqlite3, json, urllib.request, time, os
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    with urllib.request.urlopen(req,timeout=180) as r:
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

con=sqlite3.connect('file:'+r'F:\trilium\trilium-data\document.db'+'?mode=ro', uri=True)
cur=con.cursor()
help_id={}
for nid,dn in cur.execute("""SELECT n.noteId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'""").fetchall():
    help_id[dn]=nid
con.close()

# 修正映射
fixes={
 'WYTkBSXUaU4m':'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Evernote',
 'RnGPOFzZTT85':'User Guide/User Guide/Scripting/Script API',
 'stK1EEQ7I07S':'User Guide/User Guide/Scripting/Script API',
}
for noteid,dn in fixes.items():
    hid=help_id.get(dn)
    if not hid:
        print('NO HELP ID:', dn); continue
    content=call('get_note_content', {'noteId':hid})
    try: md=json.loads(content).get('content','')
    except: md=content
    with open(os.path.join('_help_md', noteid+'.md'),'w',encoding='utf-8') as f:
        f.write(md)
    print(f'{noteid}: {dn} len={len(md)} helpId={hid}')
    # 同步修正 _help_md_index.json
    time.sleep(0.1)
