# -*- coding: utf-8 -*-
import sqlite3, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 检查特定空内容笔记对应的 _help 节点是否有 docName
ids=['VugMPu4jc7IZ','hWMN4ej0xMh3','jFXOYH8BO8zS','20c3yUXh3jcZ','BROicBbRhY1u','5JQ4vkZqncQO','yzyWwxCeBHm2','kBQZqj9dubVL','Ht8ZUwhpA1DH']
for nid in ids:
    r=cur.execute("SELECT title FROM notes WHERE noteId=?", (nid,)).fetchone()
    title=r[0] if r else '?'
    # 找它可能对应的 _help 节点（标题匹配）
    help_rows=cur.execute("SELECT n.noteId, n.title, a.value FROM notes n JOIN attributes a ON a.noteId=n.noteId AND a.name='docName' AND a.isDeleted=0 WHERE n.isDeleted=0 AND n.noteId LIKE '_help_%' AND (n.title LIKE ? OR a.value LIKE ?)", (f'%{title}%', f'%{title}%')).fetchall()
    print(f'{nid} | {title}')
    for hr in help_rows[:5]:
        print(f'    _help: {hr[0]} | {hr[1]} | {hr[2]}')
    if not help_rows:
        print('    无匹配 _help')
