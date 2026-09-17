# -*- coding: utf-8 -*-
import sqlite3, json, urllib.request, time, os
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    try:
        with urllib.request.urlopen(req,timeout=180) as r:
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
        res=out[-1].get('result',{})
        content=res.get('content')
        txt=''
        if isinstance(content,list):
            for c in content:
                if isinstance(c,dict): txt+=c.get('text','')
        return txt
    return ''

# _help docName -> noteId
con=sqlite3.connect('file:'+r'F:\trilium\trilium-data\document.db'+'?mode=ro', uri=True)
cur=con.cursor()
help_id={}
for nid,dn in cur.execute("""SELECT n.noteId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'""").fetchall():
    help_id[dn]=nid
con.close()

issues=json.load(open('_book_has_content.json',encoding='utf-8'))
os.makedirs('_help_md', exist_ok=True)
out=[]
for it in issues:
    dn=it['src_docName']
    hid=help_id.get(dn)
    if not hid:
        print('NO HELP ID:', dn); continue
    content=call('get_note_content', {'noteId':hid})
    if content.startswith('ERR:'):
        print('MCP ERR:', dn, content[:100]); continue
    # content 是 JSON 字符串 {"noteId":..., "content":"..."} 或纯文本?
    try:
        j=json.loads(content)
        md=j.get('content','')
    except:
        md=content
    out.append({'noteId':it['book_noteId'],'title':it['book_title'],'docName':dn,'helpId':hid,'md':md,'len':len(md)})
    with open(os.path.join('_help_md', it['book_noteId']+'.md'),'w',encoding='utf-8') as f:
        f.write(md)
    time.sleep(0.1)
print('读取完成:', len(out))
total=sum(o['len'] for o in out)
print('总字符:', total, '平均:', total//max(len(out),1))
json.dump([{'noteId':o['noteId'],'title':o['title'],'docName':o['docName'],'len':o['len']} for o in out],
          open('_help_md_index.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
