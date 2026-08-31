# -*- coding: utf-8 -*-
import json
header=json.load(open('_asar_header.json',encoding='utf-8'))
print(type(header))
print('top keys:', list(header.keys()))
files=header['files']
print('files type:', type(files), list(files.keys())[:10] if isinstance(files,dict) else len(files))
assets=files.get('assets')
print('assets type:', type(assets), assets.keys() if isinstance(assets,dict) else '')
print('assets.files keys:', list(assets.get('files',{}).keys()))
print('db keys:', list(assets.get('files',{}).get('db',{}).keys()))
