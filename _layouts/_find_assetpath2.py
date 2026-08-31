# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
text=data[data_start+off:data_start+off+size].decode('utf-8','replace')

# Gbe receives 'e' as assetPath; find where Gbe is called
for kw in ['Gbe(', 'Gbe=', 'uN', 'function Gbe', 'render("app"', 'assetPath:uN', 'assetPath: uN', 'app:uN', 'app: uN']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    print(f'\n== {kw}: {len(idxs)} ==')
    for i in idxs[:6]:
        seg=text[max(0,i-160):i+200].replace(chr(10),' ')
        print('  ...', seg[:360])
