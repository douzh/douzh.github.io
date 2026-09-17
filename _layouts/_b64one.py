# -*- coding: utf-8 -*-
# base64 编码输出，避免编码问题
import base64, sys
fn=sys.argv[1]
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_help_md\%s.md'%fn,'rb') as f:
    data=f.read()
b64=base64.b64encode(data).decode('ascii')
sys.stdout.write(b64)
