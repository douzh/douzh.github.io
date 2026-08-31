# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
text=data[data_start+off:data_start+off+size].decode('utf-8','replace')

# find ZI definition
for kw in ['ZI=', 'var ZI', 'ZI =', 'const ZI', 'ZI=']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    print(f'== {kw}: {len(idxs)} ==')
    for i in idxs[:5]:
        print('  ...', text[max(0,i-80):i+120].replace(chr(10),' ')[:200])

# find express listen / port
for kw in ['listen(', 'TRILIUM_PORT', '8080', '37740', '37840']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    print(f'\n== {kw}: {len(idxs)} ==')
    for i in idxs[:6]:
        print('  ...', text[max(0,i-100):i+120].replace(chr(10),' ')[:220])
