# -*- coding: utf-8 -*-
# 读取 43 个源 HTML → 转 markdown → 保存供翻译
import json, re, os
import html2text
asar=r'F:\trilium\Trilium\resources\app.asar'
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data_start=772056

def read_file(path):
    node=h
    for part in path.split('/'):
        if 'files' not in node or part not in node['files']:
            return None
        node=node['files'][part]
    if 'offset' not in node: return None
    with open(asar,'rb') as f:
        f.seek(data_start+int(node['offset']))
        return f.read(int(node['size']))

# 找图片二进制起始: JPEG(FFD8FF) 或 PNG(89504E47)
def split_html(data):
    for sig in (b'\xff\xd8\xff', b'\x89PNG', b'<svg'):
        idx=data.find(sig)
        if idx>0:
            return data[:idx]
    return data

def md_of(html_text):
    h2t=html2text.HTML2Text()
    h2t.body_width=0
    h2t.ignore_images=False
    h2t.ignore_links=False
    h2t.unicode_snob=True
    return h2t.handle(html_text)

issues=json.load(open('_book_has_content.json',encoding='utf-8'))
os.makedirs('_src_md', exist_ok=True)
out=[]
for it in issues:
    dn=it['src_docName']
    path='assets/doc_notes/en/'+dn+'.html'
    data=read_file(path)
    if not data:
        print('MISS:', dn); continue
    text=split_html(data)
    try:
        html_text=text.decode('utf-8','replace')
    except: 
        print('DECODE FAIL:', dn); continue
    md=md_of(html_text)
    # 清理: 合并多余空行
    md=re.sub(r'\n{3,}','\n\n',md)
    out.append({'noteId':it['book_noteId'],'title':it['book_title'],'docName':dn,'md':md})
    with open(os.path.join('_src_md', it['book_noteId']+'.md'),'w',encoding='utf-8') as f:
        f.write(md)
print('转换完成:', len(out))
json.dump([{'noteId':o['noteId'],'title':o['title'],'docName':o['docName'],'md_len':len(o['md'])} for o in out],
          open('_src_md_index.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
