# -*- coding: utf-8 -*-
import base64
for fn in ['Gh7vim8mU3XG','iLOq7QKFCwlZ','B5oluBvu2gBY','WYTkBSXUaU4m']:
    with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\%s.md'%fn,'rb') as f:
        data=f.read()
    print('===== %s len=%d ====='%(fn,len(data)))
    print(data.decode('utf-8')[:200])
    print()
