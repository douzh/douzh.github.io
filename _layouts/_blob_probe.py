# -*- coding: utf-8 -*-
import sqlite3
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()
nid='Q0A1Gg3pgIOP'
n=cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
print('note:', n)
b=cur.execute("SELECT blobId, content, LENGTH(content) FROM blobs WHERE blobId=?", (n[2],)).fetchone()
print('blob row:', (b[0], b[2]) if b else None)
if b and b[1]:
    data=b[1]
    print('前32字节hex:', data[:32].hex())
    # 判断是否 utf-8 文本
    try:
        txt=data.decode('utf-8')
        print('utf8 文本长度:', len(txt))
        print('预览:', txt[:300])
    except UnicodeDecodeError as e:
        print('非utf8, 可能是压缩:', e)
        # 尝试 gzip
        import zlib
        try:
            d=zlib.decompress(data)
            print('zlib解压成功, 长度:', len(d), '预览:', d[:200].decode('utf-8','replace'))
        except Exception as e2:
            print('zlib失败:', e2)
