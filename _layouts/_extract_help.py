# -*- coding: utf-8 -*-
import json, re
header=json.load(open('_asar_header.json',encoding='utf-8'))

def walk_files(node, p='', out=None):
    if out is None: out=[]
    if isinstance(node, dict):
        if 'files' in node and isinstance(node['files'], dict):
            for name, sub in node['files'].items():
                walk_files(sub, p+'/'+name, out)
        elif 'offset' in node:
            out.append((p, int(node['offset']), node.get('size')))
    return out

assets_files=header['files']['assets']['files']
doc=walk_files(assets_files.get('doc_notes',{}), '/assets/doc_notes')
tray=[f for f in doc if f[0].endswith('Tray icon & automatic startup.html')][0]
print('target:', tray)

# extract file from asar
path=r'F:\trilium\Trilium\resources\app.asar'
data=open(path,'rb').read()
# find json header end: offset of first file data. Compute: header json starts at 16.
# We'll locate file by searching for a unique pattern is hard; instead use header offset + base
# asar v3 layout: [4B pickle len][4B json len][8B?][json][files]. Actually from earlier:
# first 8 bytes: 04 00 00 00 | c4 c7 0b 00 (=772036 json_size) then bytes ba c7 0b 00 ...
# The json starts at offset 16 per _scan_asar (found '{"files":' at idx 16). json length = 772036.
# Then after json: 8-byte footer? Standard asar v2: [4B headerSize][4B jsonSize][json][headerSize+jsonSize+8 ... files]
# headerSize field=4? That was weird. Let's compute: data[0:4]=4, data[4:8]=772036.
# The pickle header total = 4 + 4 + 8 = 16 bytes? Actually asar v2 header is 8 bytes then json, but there's also 4-byte size of json at start.
# Let's just find offset of file by brute force: file data starts after json + 8 bytes (uint32 json len + uint32...).
off, size = tray[1], tray[2]
# try to locate content by scanning: we know data offset fields are absolute within file payload.
# The 'offset' field in asar v3 is the offset within the asar file itself (absolute), so just read at off.
blob=data[off:off+size]
print('blob len:', len(blob))
print(blob.decode('utf-8','replace'))
