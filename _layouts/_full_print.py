# -*- coding: utf-8 -*-
import sys
fn=sys.argv[1]
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\%s.md'%fn,'r',encoding='utf-8') as f:
    content=f.read()
print('===== %s len=%d ====='%(fn,len(content)))
print(content)
