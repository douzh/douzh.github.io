# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
print('etapi_tokens:', cur.execute("SELECT * FROM etapi_tokens").fetchall())
print()
print('config.ini:')
print(open(r'C:\Users\douzh\AppData\Roaming\trilium-data\config.ini', encoding='utf-8').read())
