# -*- coding: utf-8 -*-
import sqlite3, json, re
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

def blob_str(blob_id):
    if not blob_id: return ''
    r=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blob_id,)).fetchone()
    if not r or not r[0]: return ''
    c=r[0]
    if isinstance(c,bytes):
        try: c=c.decode('utf-8','replace')
        except: return ''
    return c

def check(nid, label):
    n=cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c=blob_str(n[2]) if n else ''
    has_p = bool(re.search(r'<p[ >]', c))
    has_br = bool(re.search(r'<br', c))
    has_h2 = bool(re.search(r'<h[1-6][ >]', c))
    has_nl = '\n' in c
    has_md_h = bool(re.search(r'^##\s', c, re.M))
    print(f'\n=== {label} ({nid}) type={n[1] if n else "?"} len={len(c)} ===')
    print(f'  含<p>: {has_p} | 含<br>: {has_br} | 含<h>: {has_h2} | 含\\n: {has_nl} | 含markdown##: {has_md_h}')
    print('  预览:', repr(c[:250]))

# 刚改类型的中间层
check('Q0A1Gg3pgIOP', '笔记类型(改类型)')
check('B5oluBvu2gBY', '文本(改类型)')
# 叶子（正常工作的）
check('6at3wkEnLIfu', '快速开始(叶子)')
check('NRzmLnGiDb8U', '内部链接(叶子)')
check('Td5XP4paGGvm', 'FAQ(叶子)')
# 检查子笔记视图相关属性
print('\n=== 子笔记视图属性 ===')
rows=cur.execute("SELECT DISTINCT name FROM attributes WHERE name IN ('childView','view','hideChildrenOverview','hidePromotedAttributes','viewType')").fetchall()
print('相关属性名:', [r[0] for r in rows])
