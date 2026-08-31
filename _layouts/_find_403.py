# -*- coding: utf-8 -*-
import json, re, struct
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()
jsn=struct.unpack('<I', data[4:8])[0]
data_start=16+jsn+4
mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
text=data[data_start+off:data_start+off+size].decode('utf-8','replace')

# Find express app setup: look for app.use middleware chains, 403 responses, 'Forbidden'
# Search for how the server handles requests that aren't from the app - look for e.g. 'Kbi' or global middleware
for kw in ['set("etag"', 'app.set', 'express()', 'use((', 'use(function', 'status(403)', 'sendStatus(403)', 'forbidden', 'Forbidden', 'checkCsrf', 'CsrfProtection', 'doubleCsrfProtection', 'isAllowedHost', 'allowedHost', 'Host header']:
    idxs=[m.start() for m in re.finditer(re.escape(kw), text)]
    if idxs:
        print(f'\n== {kw}: {len(idxs)} ==')
        for i in idxs[:5]:
            print('  ...', text[max(0,i-140):i+180].replace(chr(10),' ')[:320])
