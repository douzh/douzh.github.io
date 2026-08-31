# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
json_start=16
data_start=json_start+jsn+4
mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
text=data[data_start+off:data_start+off+size].decode('utf-8','replace')

print('main.cjs size:', len(text))
# search for doc handling
for kw in ['doc_notes','docUrl','docName','helpRoot','docLoader','/api/doc','doc/notes','_image.png','helpSearch']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    print(f'\n== {kw}: {len(idxs)} hits ==')
    for i in idxs[:4]:
        print('  ...', text[max(0,i-150):i+220].replace(chr(10),' ')[:360])
