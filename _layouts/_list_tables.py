# -*- coding: utf-8 -*-
import sqlite3
conn = sqlite3.connect(r'F:\trilium\trilium-data\document.db')
for r in conn.execute("SELECT name FROM sqlite_master WHERE type='table'").fetchall():
    print(r[0])
conn.close()
