# -*- coding: utf-8 -*-
import json, re
mp = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_id_map.json', encoding='utf-8'))
# 提取 4 个源中的 _help 引用
sources = {
 '_help_mHbBMPDPkVV5': r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\_help_mHbBMPDPkVV5.md',
 '_help_pgxEVkzLl1OP': r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\_help_pgxEVkzLl1OP.md',
 '_help_uYF7pmepw27K': r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\_help_uYF7pmepw27K.md',
 '_help_W8vYD3Q1zjCR': r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\_help_W8vYD3Q1zjCR.md',
}
for src, path in sources.items():
    txt = open(path, encoding='utf-8').read()
    refs = set(re.findall(r'#root/(_help_[A-Za-z0-9]+)', txt))
    missing = [(r, mp.get(r)) for r in refs if mp.get(r) is None]
    print('==', src, '引用', len(refs))
    for r in refs:
        print('   ', r, '->', mp.get(r, '!! 无映射'))
