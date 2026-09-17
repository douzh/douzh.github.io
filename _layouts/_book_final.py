# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_str(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    c=r[0]
    if isinstance(c,bytes):
        try: c=c.decode('utf-8','replace')
        except: return ''
    return c

issues=json.load(open('_book_issues.json',encoding='utf-8'))
print('book 问题节点:', len(issues))

has=[]; empty=[]
for i in issues:
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (i['book_noteId'],)).fetchone()
    txt=blob_str(n[0]) if n else ''
    import re
    plain=re.sub(r'<[^>]+>',' ',txt)
    plain=re.sub(r'\s+',' ',plain).strip()
    i['plain_len']=len(plain)
    if plain:
        has.append(i)
    else:
        empty.append(i)

print(f'有双语内容但类型为 book(显示为网格/列表): {len(has)}')
print(f'真正空(无内容): {len(empty)}')
for i in empty:
    print('  空:', i['book_title'], '<<源>>', i['src_title'])

print('\n=== 完整清单: 有内容但类型错误 ===')
has.sort(key=lambda x:x['src_docName'])
for idx,i in enumerate(has,1):
    print(f"{idx:2d}. {i['book_title']}  (内容 {i['plain_len']} 字)  <<源>> {i['src_title']}")
json.dump(has, open('_book_has_content.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
