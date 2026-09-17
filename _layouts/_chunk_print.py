# -*- coding: utf-8 -*-
import sys
fn='Gh7vim8mU3XG'
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\%s.md'%fn,'r',encoding='utf-8') as f:
    content=f.read()
# 分块打印，每块 1200 字符
chunk=1200
for i in range(0,len(content),chunk):
    sys.stdout.write('=== %d ===\n'%(i//chunk))
    sys.stdout.write(content[i:i+chunk]+'\n')
