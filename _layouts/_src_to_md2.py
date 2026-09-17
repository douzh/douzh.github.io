# -*- coding: utf-8 -*-
import sqlite3, json, re, os
import html2text
asar=r'F:\trilium\Trilium\resources\app.asar'
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data_start=772056

def read_file(path):
    node=h
    for part in path.split('/'):
        if 'files' not in node or part not in node['files']: return None
        node=node['files'][part]
    if 'offset' not in node: return None
    with open(asar,'rb') as f:
        f.seek(data_start+int(node['offset']))
        return f.read(int(node['size']))

def split_html(data):
    for sig in (b'\xff\xd8\xff', b'\x89PNG', b'<svg'):
        idx=data.find(sig)
        if idx>0: return data[:idx]
    return data

def md_of(html_text):
    h2t=html2text.HTML2Text()
    h2t.body_width=0
    h2t.ignore_images=False
    h2t.ignore_links=False
    h2t.unicode_snob=True
    return h2t.handle(html_text)

# 1) 建 _help noteId -> 用户指南 noteId 映射 (通过 docName)
con=sqlite3.connect('file:'+r'F:\trilium\trilium-data\document.db'+'?mode=ro', uri=True)
cur=con.cursor()
# _help 节点 docName
help_map={}
for nid,dn in cur.execute("""SELECT n.noteId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0
    WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%'""").fetchall():
    help_map[dn]=nid
# 用户指南节点 docPath
ug_map={}
for nid,dp in cur.execute("""SELECT n.noteId, a.value FROM notes n
    JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0
    WHERE n.isDeleted=0""").fetchall():
    ug_map[dp]=nid
con.close()
# 映射 _help_noteId -> ug_noteId
id_map={}
for dn,hid in help_map.items():
    if dn in ug_map:
        id_map[hid]=ug_map[dn]
print('可映射 _help->UG 链接:', len(id_map))

# 2) 转换 42 篇
issues=json.load(open('_book_has_content.json',encoding='utf-8'))
os.makedirs('_src_md', exist_ok=True)
n_link=0; n_img=0
for it in issues:
    dn=it['src_docName']
    data=read_file('assets/doc_notes/en/'+dn+'.html')
    if not data: print('MISS:', dn); continue
    html_text=split_html(data).decode('utf-8','replace')
    # 预处理: 内部链接换占位
    def repl_link(m):
        global n_link
        n_link+=1
        return 'href="https://root.local/'+m.group(1).lstrip('#root/')+'"'
    html_text=re.sub(r'href="#root/([^"]+)"', repl_link, html_text)
    md=md_of(html_text)
    # 还原链接 + 修图片空格
    md=re.sub(r'\]\((https://root\.local/)([^)]+)\)', lambda m: '](#root/'+m.group(2)+')', md)
    md=re.sub(r'!\[([^\]]*)\]\(([^)]+)\)', lambda m: '!['+m.group(1)+']('+m.group(2).replace(' ','%20')+')', md)
    md=re.sub(r'\n{3,}','\n\n',md)
    with open(os.path.join('_src_md', it['book_noteId']+'.md'),'w',encoding='utf-8') as f:
        f.write(md)
print('链接数:', n_link)
print('完成转换')

# 3) 保存 id_map
json.dump(id_map, open('_id_map.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
