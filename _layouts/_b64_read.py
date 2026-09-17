# -*- coding: utf-8 -*-
import base64
for fn in ['Gh7vim8mU3XG','iLOq7QKFCwlZ','B5oluBvu2gBY']:
    with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\%s.md'%fn,'rb') as f:
        data=f.read()
    b64=base64.b64encode(data).decode('ascii')
    import sys
    sys.stdout.write('\n===== %s len=%d =====\n'%(fn,len(data)))
    # 分块打印 base64，每块 4000
    for i in range(0,len(b64),4000):
        sys.stdout.write(b64[i:i+4000]+'\n')
