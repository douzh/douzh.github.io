# -*- coding: utf-8 -*-
"""检查 25 篇源 md 引用的 _help_ 链接，核对 _id_map.json 映射"""
import json, re, glob, os
idmap = json.load(open('_id_map.json', encoding='utf-8'))
print('_id_map.json 共 %d 条' % len(idmap))
refs = set()
for fn in glob.glob('_leaf_src/*.md'):
    c = open(fn, encoding='utf-8').read()
    for m in re.findall(r'#root/(_help_[A-Za-z0-9]+)', c):
        refs.add(m)
print('源 md 引用 _help_ id 共 %d 个' % len(refs))
missing = [r for r in refs if r not in idmap]
print('缺失映射 %d 个:' % len(missing))
for r in sorted(missing):
    print('  ', r)
# 展示已有映射示例
print('\n已映射示例:')
for r in sorted(refs):
    if r in idmap:
        print('  %s -> %s' % (r, idmap[r]))
