# -*- coding: utf-8 -*-
import sqlite3
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
cur = conn.cursor()
cur.execute("UPDATE notes SET type='text' WHERE noteId IN ('BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM')")
conn.commit()
print('rows:', cur.rowcount)
cur.execute("SELECT noteId,type FROM notes WHERE noteId IN ('BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM')")
print(cur.fetchall())
conn.close()
