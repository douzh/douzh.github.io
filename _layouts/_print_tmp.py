# -*- coding: utf-8 -*-
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_tmp_md.txt','r',encoding='utf-8') as f:
    lines=f.readlines()
print('LINES:', len(lines))
for i,l in enumerate(lines):
    # 转成可打印: 替换非 ascii 为转义
    safe=''.join(ch if ord(ch)<128 else ('\\u%04x'%ord(ch)) for ch in l)
    print(f'{i}: {safe[:200]}')
