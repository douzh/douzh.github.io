# -*- coding: utf-8 -*-
import sqlite3, json, time, os

db=r'F:\trilium\trilium-data\document.db'
bak=r'F:\trilium\trilium-data\document.db.bak_20260917_book2text'
issues=json.load(open('_book_issues.json',encoding='utf-8'))
ids=[i['book_noteId'] for i in issues]
print('待改类型笔记:', len(ids))

# 1) 一致性备份（含 WAL 内容）
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True, timeout=30)
b=sqlite3.connect(bak, timeout=30)
with b:
    con.backup(b)
b.close(); con.close()
print('备份完成:', bak, os.path.getsize(bak), 'bytes')

# 2) 改类型（writable 连接）
con=sqlite3.connect(db, timeout=60)
con.execute('PRAGMA busy_timeout=60000')
cur=con.cursor()
placeholders=','.join('?'*len(ids))
cur.execute(f"UPDATE notes SET type='text' WHERE noteId IN ({placeholders})", ids)
print('受影响行数:', cur.rowcount)
con.commit()

# 3) 验证
rows=cur.execute(f"SELECT noteId, title, type FROM notes WHERE noteId IN ({placeholders})", ids).fetchall()
con.close()
bad=[r for r in rows if r[2]!='text']
print('验证: 总数', len(rows), '非text', len(bad))
for r in rows[:5]: print(' ', r)
