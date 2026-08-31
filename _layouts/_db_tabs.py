# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
tabs=[r[0] for r in cur.execute("SELECT name FROM sqlite_master WHERE type='table'").fetchall()]
print('tables:', tabs)
