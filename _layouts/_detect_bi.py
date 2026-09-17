# -*- coding: utf-8 -*-
import sqlite3, re
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

def get_blob(blobId):
    row = cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()
    return row[0] if row else None

def subtree(note_id, depth=0):
    rows = cur.execute("""
      SELECT b.noteId, n.title, n.type, n.blobId
      FROM branches b JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId = ? ORDER BY b.notePosition, b.noteId
    """, (note_id,)).fetchall()
    out = []
    for r in rows:
        out.append((r[0], r[1], r[2], depth))
        out += subtree(r[0], depth+1)
    return out

# 双语检测：块级内容里存在"英文句子 中文句子"模式（英文单词数>40且中文>40且英文在中文前）
def is_bilingual_html(c):
    if not c or not re.search(r'<p', c):
        return False
    # 提取所有块文本
    blocks = re.findall(r'<(?:p|li|h[1-6]|td|th|figcaption|dt|dd)[^>]*>(.*?)</(?:p|li|h[1-6]|td|th|figcaption|dt|dd)>', c, re.S)
    for b in blocks:
        txt = re.sub(r'<[^>]+>', ' ', b)
        cn = len(re.findall(r'[\u4e00-\u9fff]', txt))
        en = len(re.findall(r'[A-Za-z]{3,}', txt))
        if cn > 40 and en > 40:
            # 英文是否主要出现在中文之前
            first_cn = re.search(r'[\u4e00-\u9fff]', txt)
            if first_cn:
                before = txt[:first_cn.start()]
                en_before = len(re.findall(r'[A-Za-z]{3,}', before))
                if en_before > 25:
                    return True
    return False

print('=== 桌面安装 ===')
for nid, title, typ, depth in subtree('39unKGhUFGBr'):
    row = cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = get_blob(row[0]) if row else ''
    if typ == 'text':
        print('%s | %s | 双语=%s' % (nid, (title or '')[:50], is_bilingual_html(c)))

print('\n=== 服务器安装 ===')
for nid, title, typ, depth in subtree('z5TN5NCWzga3'):
    row = cur.execute("SELECT blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = get_blob(row[0]) if row else ''
    if typ == 'text':
        print('%s | %s | 双语=%s' % (nid, (title or '')[:50], is_bilingual_html(c)))
conn.close()
