# -*- coding: utf-8 -*-
import json
h=json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_asar_header.json',encoding='utf-8'))
data=open(r'F:\trilium\Trilium\resources\app.asar','rb').read()

# asar v3: [u32 header_len][u32 json_len][pickle...][json][u32 json_len][data...]
import struct
hdr=struct.unpack('<I', data[0:4])[0]
jsn=struct.unpack('<I', data[4:8])[0]
print('hdr field:', hdr, 'json field:', jsn)
# json starts at 16 based on earlier scan
json_start=16
json_bytes=data[json_start:json_start+jsn]
print('json len match:', len(json_bytes)==jsn)
# data starts after json + 4 (footer u32)
data_start=json_start+jsn+4
print('data_start:', data_start)

mc=h['files']['main.cjs']
off=int(mc['offset']); size=int(mc['size'])
blob=data[data_start+off:data_start+off+size]
print('main.cjs blob len:', len(blob))
print('head:', blob[:120])
