# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_text(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    try: return r[0].decode('utf-8', 'replace')
    except: return ''

issues=json.load(open('_book_issues.json',encoding='utf-8'))
print('book 问题节点:', len(issues))

bl_pos=bl_zero=0
for i in issues:
    # book 自身内容
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (i['book_noteId'],)).fetchone()
    txt=blob_text(n[0]) if n and n[0] else ''
    i['blob_preview']=txt[:200].replace('\n',' ')
    if txt.strip():
        bl_pos+=1
        # 去掉 html 标签看纯文本长度
        import re
        plain=re.sub(r'<[^>]+>',' ',txt)
        plain=re.sub(r'\s+',' ',plain).strip()
        i['plain_len']=len(plain)
    else:
        bl_zero+=1
        i['plain_len']=0
print(f'book 有 blob 内容: {bl_pos}, 完全空: {bl_zero}')

print('\n=== 有内容但类型为 book(内容无法渲染) ===')
for i in issues:
    if i['plain_len']>0:
        print(f"  [{i['plain_len']:5d}字] {i['book_title']}  <<源>> {i['src_title']}")

print('\n=== 完全空 book ===')
for i in issues:
    if i['plain_len']==0:
        print(f"  [空] {i['book_title']}  <<源>> {i['src_title']}")

# 抽样看 笔记类型 的内容
for i in issues:
    if i['book_title']=='笔记类型':
        print('\n=== 笔记类型 blob 内容预览 ===')
        print(i['blob_preview'])
