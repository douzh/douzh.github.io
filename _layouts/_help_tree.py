# -*- coding: utf-8 -*-
"""查 _help_ 树的父子层级，确认安装章节各叶子的正确源节点；读取源内容长度"""
import sqlite3, json
DB = r'F:\trilium\trilium-data\document.db'
conn = sqlite3.connect(DB)
conn.text_factory = lambda b: b.decode('utf-8', 'replace')
cur = conn.cursor()

# 构建 _help_ 树
def children_of(pid):
    return cur.execute("""
      SELECT b.noteId, n.title, n.type, n.blobId FROM branches b
      JOIN notes n ON b.noteId = n.noteId
      WHERE b.parentNoteId=? AND n.isDeleted=0 ORDER BY b.notePosition
    """, (pid,)).fetchall()

def show_tree(pid, depth=0, maxd=3):
    for r in children_of(pid):
        print('  '*depth + '%s | %s | %s' % (r[0], (r[1] or '')[:50], r[2]))
        if depth < maxd:
            show_tree(r[0], depth+1, maxd)

print('=== Installation & Setup 树 ===')
show_tree('_help_Otzi9La2YAUX', 0, 3)

print('\n=== 读取关键源节点内容长度 ===')
ids = ['_help_nRqcgfTb97uV','_help_Rp0q8bSP6Ayl','_help_iGTnKjubbXkA','_help_swSFivWk6KkA',
       '_help_7iwde4lsKc6O','_help_hvRfsDr591WP','_help_3tW6mORuTHnB','_help_rWX5eY045zbE',
       '_help_moVgBcoxE3EK','_help_J1Bb6lVlwU5T','_help_DCmT6e7clMoP','_help_klCWNks3ReaQ',
       '_help_vcjrb3VVYPZI','_help_ud6MShXL4WpO','_help_fDLvzOx29Pfg','_help_LLzSMXACKhUs',
       '_help_5ERVJb9s4FRD','_help_l2VkvOwUNfZj','_help_0hzsNCP31IAB','_help_lwAX5SvbrK2g',
       '_help_7DAiwaf8Z7Rz','_help_Un4wj2Mak2Ky','_help_x1lQNKQBqLcA','_help_IgkTHSaFfq6A',
       '_help_yeEaYqosGLSh','_help_bkPZAMT7d2sR']
for i in ids:
    r = cur.execute("SELECT title, type, blobId FROM notes WHERE noteId=?", (i,)).fetchone()
    if not r:
        print('%s | 不存在' % i); continue
    b = cur.execute("SELECT content FROM blobs WHERE blobId=?", (r[2],)).fetchone()
    print('%s | %s | %s | len=%d' % (i, (r[0] or '')[:50], r[1], len(b[0]) if b else 0))
conn.close()
