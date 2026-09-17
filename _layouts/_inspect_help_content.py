# -*- coding: utf-8 -*-
import json
d=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\help_content.json',encoding='utf-8'))
print('type:', type(d))
if isinstance(d, dict):
    print('keys count:', len(d))
    for k in list(d.keys())[:5]:
        print(' key:', repr(k), '->', str(d[k])[:150])
elif isinstance(d, list):
    print('list len:', len(d))
    print(json.dumps(d[0], ensure_ascii=False)[:400])
