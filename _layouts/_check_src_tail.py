# -*- coding: utf-8 -*-
import json, re, sqlite3
asar=r'F:\trilium\Trilium\resources\app.asar'
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data_start=772056
def read_file(path):
    node=h
    for part in path.split('/'):
        if 'files' not in node or part not in node['files']:
            return None
        node=node['files'][part]
    if 'offset' not in node: return None
    with open(asar,'rb') as f:
        f.seek(data_start+int(node['offset']))
        return f.read(int(node['size']))

html=read_file('assets/doc_notes/en/User Guide/User Guide/Note Types.html')
print('读取:', 'OK len='+str(len(html)) if html else 'FAIL')
if html:
    t=html.decode('utf-8')
    print('底部 2000 字符:')
    print(t[-2000:])
