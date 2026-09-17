# -*- coding: utf-8 -*-
# 把 tmp 内容 base64 后打印前 2000 字符，避免编码问题
import base64
with open(r'D:\mycloud\0pnbase\kb-main\_layouts\_tmp_md.txt','rb') as f:
    data=f.read()
b64=base64.b64encode(data).decode('ascii')
print('LEN:', len(data), 'B64LEN:', len(b64))
print(b64[:3000])
