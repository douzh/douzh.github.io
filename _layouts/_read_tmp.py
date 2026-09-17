# -*- coding: utf-8 -*-
import sys
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\ThwV4HVTjhHq.md','r',encoding='utf-8') as f:
    content=f.read()
sys.stdout.write('LEN='+str(len(content))+'\n')
# 写到另一个文件（排除写入问题）
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_tmp_md.txt','w',encoding='utf-8') as f:
    f.write(content)
print('ok')
