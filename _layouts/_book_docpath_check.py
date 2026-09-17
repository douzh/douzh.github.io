# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
issues=json.load(open('_book_issues.json',encoding='utf-8'))
for i in issues:
    r=cur.execute("SELECT COUNT(*) FROM attributes WHERE noteId=? AND name='docPath' AND isDeleted=0", (i['book_noteId'],)).fetchone()
    i['has']=r[0]>0
has=[i for i in issues if i['has']]
no=[i for i in issues if not i['has']]
print(f'已有 docPath: {len(has)} | 需补: {len(no)}')
for i in no:
    print(' 补:', i['book_noteId'], i['book_title'], '->', i['src_docName'])
json.dump(no, open('_book_need_docpath.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
