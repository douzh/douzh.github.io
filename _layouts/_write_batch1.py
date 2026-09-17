# -*- coding: utf-8 -*-
import json, urllib.request, time, re
URL='http://127.0.0.1:37840/mcp'
HDR={'Authorization':'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=','Content-Type':'application/json','Accept':'application/json, text/event-stream'}
_id=[0]
def call(tool, args):
    _id[0]+=1
    payload={'jsonrpc':'2.0','id':_id[0],'method':'tools/call','params':{'name':tool,'arguments':args}}
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

id_map=json.load(open('_id_map.json',encoding='utf-8'))

# 翻译内容: noteId -> markdown（中文）
TRANS={
'JmvAMQu8gb4o': '''代码片段（Snippets）让你可以定义可复用的文本、Markdown 或代码片段，并快速插入到你的笔记中。''',

'2IpMVDLsydKq': '''链接有两种类型：

*   <a class="reference-link" href="#root/_help_3IDVtesTQ8ds">外部链接（External links）</a>，用于指向网站或其他资源的标准超链接。
*   <a class="reference-link" href="#root/_help_hrZ1D00cLbal">内部（引用）链接（Internal (reference) links）</a>，用于指向 Trilium 内其他笔记的链接。
    *   指向某一特定笔记的笔记列表称为<a class="reference-link" href="#root/_help_wfGXLmm7x27z">反向链接（Backlinks）</a>。

## 笔记图

Trilium 提供了某个笔记的传入和传出链接的可视化。详情请参阅[笔记图（Note Map）](#root/_help_BCkXAVs63Ttv)。''',

'GN7LU2N7OhJi': '''Trilium 通过[脚本（Scripts）](#root/_help_CdNpE2pqjmI6)和[提升属性（Promoted Attributes）](#root/_help_OFXdgB2nNk1F)提供高级功能。为了演示这些功能，我们准备了一些在[演示笔记（demo notes）](#root/_help_wX4HbRucYSDD)中可用的示例：

*   [关系图（Relation Map）](#root/_help_iRwzGnHPzonm)
*   [每日笔记（Day Notes）](#root/_help_l0tKav7yLHGF)
*   [体重追踪器（Weight Tracker）](#root/_help_R7abl2fc6Mxi)
*   [任务管理器（Task Manager）](#root/_help_xYjQUYhpbUEW)

需要注意的是，这些示例并不是 Trilium 开箱即用的原生功能；相反，它们展示了你在 Trilium 中可以构建的内容。''',
}

ok=fail=0
for nid,md in TRANS.items():
    # 替换内部链接 id: _help_xxx -> ug id
    def repl(m):
        hid=m.group(1)
        if hid in id_map:
            return '#root/'+id_map[hid]
        return '#root/'+hid
    md2=re.sub(r'#root/(_help_[A-Za-z0-9]+)', repl, md)
    r=call('set_note_content', {'noteId':nid,'content':md2})
    if 'success' in r and '"success":true' in r or r.startswith('{"success"'):
        ok+=1
    else:
        fail+=1
        print('FAIL', nid, r[:200])
    time.sleep(0.1)
print(f'ok={ok} fail={fail}')
