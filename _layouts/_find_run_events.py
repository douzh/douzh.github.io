# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
text=data[data_start+off:data_start+off+size].decode('utf-8','replace')

# find run event labels
for kw in ['frontendStartup','mobileStartup','run=frontend','get runLabel','runLabel','isInjectedScript','"run"','getAttribute("run")','getLabelValue("run")']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    if idxs:
        print(f'\n== {kw}: {len(idxs)} ==')
        for i in idxs[:4]:
            print('  ...', text[max(0,i-130):i+180].replace(chr(10),' ')[:310])
