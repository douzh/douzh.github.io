# -*- coding: utf-8 -*-
import json
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
# main.cjs is top-level in files
mc=h['files'].get('main.cjs')
print(json.dumps(mc, indent=1)[:2000])
