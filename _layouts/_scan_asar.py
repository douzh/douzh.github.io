# -*- coding: utf-8 -*-
import json, re

path=r'F:\trilium\Trilium\resources\app.asar'
with open(path,'rb') as f:
    data=f.read()

# The header json starts at offset 16 (4+4+8) based on earlier bytes. Try locate "files" json.
# asar v3: [u32 pickle_header_len][u32 json_header_len]... actually from bytes: 04 00 00 00 = 4, then c4 c7 0b 00 = 772036 (json size?), then another ba c7 0b 00 ...
# Let's find the JSON header by locating '{"files":' occurrence
idx=data.find(b'{"files":')
print('json header at offset:', idx)
if idx>0:
    # find matching closing brace - parse using a depth scan
    depth=0; in_str=False; esc=False; end=None
    for i in range(idx, min(len(data), idx+2000000)):
        ch=data[i:i+1]
        if in_str:
            if esc: esc=False
            elif ch==b'\\': esc=True
            elif ch==b'"': in_str=False
        else:
            if ch==b'"': in_str=True
            elif ch==b'{': depth+=1
            elif ch==b'}':
                depth-=1
                if depth==0:
                    end=i+1; break
    if end:
        header=json.loads(data[idx:end].decode('utf-8','replace'))
        print('header parsed, files tree top keys:', list(header['files'].keys()))
        # recursively find files with image extensions or help
        imgfiles=[]
        def walk(node, p=''):
            if isinstance(node, dict):
                if 'files' in node:
                    for name, sub in node['files'].items():
                        walk(sub, p+'/'+name)
                elif 'offset' in node:
                    if re.search(r'\.(png|webp|jpg|jpeg|gif|svg)$', p, re.I):
                        imgfiles.append((p, node.get('size')))
        walk(header['files'])
        print('total image files in asar:', len(imgfiles))
        for f in imgfiles[:40]:
            print('  ', f)
        json.dump(header, open('_asar_header.json','w',encoding='utf-8'))
