# -*- coding: utf-8 -*-
"""通过 MCP 拉取 25 个叶子对应的 _help_ 源英文内容，存为 md"""
import json, urllib.request, os, re
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
def call(tool, args):
    payload={'jsonrpc':'2.0','id':1,'method':'tools/call','params':{'name':tool,'arguments':args}}
    req=urllib.request.Request(URL,data=json.dumps(payload).encode('utf-8'),headers=HDR,method='POST')
    with urllib.request.urlopen(req,timeout=180) as r:
        data=r.read().decode('utf-8')
    out=[]
    for line in data.splitlines():
        line=line.strip()
        if line.startswith('data:'):
            try: out.append(json.loads(line[5:].strip()))
            except: pass
    if out:
        res=out[-1].get('result',{})
        content=res.get('content')
        txt=''
        if isinstance(content,list):
            for c in content:
                if isinstance(c,dict): txt+=c.get('text','')
        return txt
    return ''

mapping = {
 'aIwhuIM42UXZ':'_help_nRqcgfTb97uV',
 'a3TKVvk1Jcuq':'_help_Rp0q8bSP6Ayl',
 'KENrlyFaUbZN':'_help_swSFivWk6KkA',
 '3GTPwnM0LXzq':'_help_7iwde4lsKc6O',
 'I9TEAj9A5NIv':'_help_hvRfsDr591WP',
 'sPaAXjVcvZZl':'_help_3tW6mORuTHnB',
 '5HwfHJFwczl9':'_help_rWX5eY045zbE',
 'SEYvbzIDbgtV':'_help_moVgBcoxE3EK',
 'fEvxRDgByx6J':'_help_J1Bb6lVlwU5T',
 'aFYvSvzqySw1':'_help_DCmT6e7clMoP',
 'fupEm5MLR0eF':'_help_klCWNks3ReaQ',
 'O4yjkaVH3YxV':'_help_vcjrb3VVYPZI',
 'o5byGpiYwkAf':'_help_ud6MShXL4WpO',
 '74oHxA6XDX7Z':'_help_fDLvzOx29Pfg',
 '9H8FKhPVYLTX':'_help_LLzSMXACKhUs',
 'mWpKWRZZLu3R':'_help_5ERVJb9s4FRD',
 '3jBts3KWOAAP':'_help_l2VkvOwUNfZj',
 'PrgLiFypVLYw':'_help_0hzsNCP31IAB',
 'eaZEEY2i4YT2':'_help_lwAX5SvbrK2g',
 'NwgMJByVeFyC':'_help_7DAiwaf8Z7Rz',
 'Ht8ZUwhpA1DH':'_help_Un4wj2Mak2Ky',
 'g0OYZmaGK1v8':'_help_x1lQNKQBqLcA',
 'v14qii09xWVc':'_help_IgkTHSaFfq6A',
 'z2cMreTyvWVz':'_help_yeEaYqosGLSh',
 '6OOBk6hUKjLW':'_help_iGTnKjubbXkA',
}
os.makedirs('_leaf_src', exist_ok=True)
idx = {}
for leaf, src in mapping.items():
    raw = call('get_note_content', {'noteId': src})
    content = ''
    try:
        j = json.loads(raw)
        content = j.get('content','')
    except Exception as e:
        content = raw
    # 去掉 markdown 中 {noteId,content} 包裹意外残留
    fn = '_leaf_src/%s.md' % leaf
    with open(fn,'w',encoding='utf-8') as f:
        f.write(content)
    idx[leaf] = {'src':src, 'len':len(content)}
    print('%s | %s | len=%d' % (leaf, src, len(content)))
with open('_leaf_src_index.json','w',encoding='utf-8') as f:
    json.dump(idx, f, ensure_ascii=False, indent=1)
print('完成，共 %d 篇' % len(mapping))
