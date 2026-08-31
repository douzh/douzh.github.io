# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
text=data[data_start+off:data_start+off+size].decode('utf-8','replace')

# search for how doc-type note is rendered - look for 'doc' note type renderer / 'DocRenderer' / 'doc-view' / 'noteType=doc' / 'loadDocContent' 
for kw in ['DocRenderer','docRenderer','renderDoc','doc-view','docView','getDoc','fetchDoc','loadDoc','"doc"','note-type-doc','docContent','doc/','noteTypeDoc','isDoc']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    if idxs:
        print(f'\n== {kw}: {len(idxs)} ==')
        for i in idxs[:5]:
            print('  ...', text[max(0,i-140):i+240].replace(chr(10),' ')[:380])
