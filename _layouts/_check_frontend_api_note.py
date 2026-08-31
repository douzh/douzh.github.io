# -*- coding: utf-8 -*-
import sqlite3, json, struct, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
# check webView node for Frontend API
rows=cur.execute("SELECT noteId, title, blobId FROM notes WHERE noteId='_help_Q2z6av6JZVWm'").fetchall()
print(rows)
# check its attributes (docUrl etc)
rows=cur.execute("SELECT name, value FROM attributes WHERE noteId='_help_Q2z6av6JZVWm' AND isDeleted=0").fetchall()
print('attrs:', rows)
