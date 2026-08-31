# -*- coding: utf-8 -*-
import struct, json, os

path=r'F:\trilium\Trilium\resources\app.asar'
with open(path,'rb') as f:
    data=f.read()
print('file size:', len(data))
# asar v2: 8-byte header: 4 bytes size(header string json len), 4 bytes json size, then json header
# actually: first 4 bytes = size of following (4 + jsonlen + 8)? Let's parse standard asar v2
# format: [u32 header_size][u32 json_header_size][json][u32? ...][files]
pickle=data[0:8]
print('first 8 bytes:', pickle.hex())
# asar v2 header: 4 bytes little-endian = length of header string (json) + padding
header_size=struct.unpack('<I', data[0:4])[0]
print('header_size field:', header_size)
json_size=struct.unpack('<I', data[4:8])[0]
print('json_size field:', json_size)
# try reading json
try:
    header=json.loads(data[8:8+json_size].decode('utf-8','replace'))
    print('parsed json header keys:', list(header.keys()))
except Exception as e:
    print('json parse error:', e)
    print(data[8:200])
