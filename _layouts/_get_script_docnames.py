# -*- coding: utf-8 -*-
import sqlite3, json, struct, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
# get docName for these help notes then read from asar
rows=cur.execute("SELECT noteId, value FROM attributes WHERE isDeleted=0 AND type='label' AND name='docName' AND noteId IN ('_help_d3fAXQ2diepH','_help_CdNpE2pqjmI6','_help_SPirpZypehBG','_help_GLks18SNjxmC','_help_yIhgI5H7A2Sm','_help_irkwjdNtjcxR')").fetchall()
for r in rows:
    print(r)
