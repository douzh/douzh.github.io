# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
ids=[i['book_noteId'] for i in json.load(open('_book_issues.json',encoding='utf-8'))]
ph=','.join('?'*len(ids))
rows=cur.execute(f"SELECT n.noteId, n.title, n.type, a.value FROM notes n LEFT JOIN attributes a ON a.noteId=n.noteId AND a.name='docPath' AND a.isDeleted=0 WHERE n.noteId IN ({ph})", ids).fetchall()
print(f'总数: {len(rows)} | type=text: {sum(1 for r in rows if r[2]=="text")} | 有docPath: {sum(1 for r in rows if r[3])}')
missing=[r for r in rows if not r[3]]
wrongtype=[r for r in rows if r[2]!='text']
if missing: print('缺 docPath:', [r[1] for r in missing])
if wrongtype: print('非 text:', [r[1] for r in wrongtype])
