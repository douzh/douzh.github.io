# -*- coding: utf-8 -*-
import json
book = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_book_issues.json', encoding='utf-8'))
print('book_issues 条数:', len(book))
for it in book:
    print('%s | %s | src=%s' % (it.get('book_noteId'), it.get('title'), it.get('src_docName')))
print()
# 检查这几个是否在清单里
for nid in ['BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM','RC8UiNCM0fHL']:
    hit = [it for it in book if it.get('book_noteId')==nid]
    print(nid, '->', hit if hit else '不在43清单中')
