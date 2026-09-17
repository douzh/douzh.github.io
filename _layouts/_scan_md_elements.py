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

issues=json.load(open('_book_has_content.json',encoding='utf-8'))
# 全部内容拼接扫描元素
all_txt=''
for i in issues:
    n=cur.execute("SELECT blobId FROM notes WHERE noteId=?", (i['book_noteId'],)).fetchone()
    all_txt+=blob_str(n[0])+'\n'
print('总内容长度:', len(all_txt))

checks={
  '**粗体**': r'\*\*[^*]+\*\*',
  '*斜体*': r'(?<!\*)\*[^*\n]+\*(?!\*)',
  '`行内码`': r'`[^`\n]+`',
  '[链接](url)': r'\[[^\]]+\]\([^)]+\)',
  '![图片](url)': r'!\[[^\]]*\]\([^)]+\)',
  '表格|': r'^\s*\|.*\|\s*$',
  '列表* ': r'^\s*\*\s+',
  '列表- ': r'^\s*-\s+',
  '数字列表': r'^\s*\d+\.\s+',
  '引用>': r'^\s*>\s?',
  '### 标题': r'^###\s',
  '#### 标题': r'^####\s',
  '代码块```': r'^```',
  '已有<a>标签': r'<a\s',
  '已有<code>': r'<code>',
  '已有<strong>': r'<strong>',
  '已有<ul>': r'<ul>',
  '已有<p>': r'<p>',
  '已有<h': r'<h[1-6]>',
  '&nbsp': r'&nbsp;',
  '<img': r'<img',
}
for name,pat in checks.items():
    m=re.findall(pat, all_txt, re.M)
    print(f'{name}: {len(m)}')
    if m and len(m)<=3:
        print('   例:', m[:3])
