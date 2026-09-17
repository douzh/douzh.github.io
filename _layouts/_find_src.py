# -*- coding: utf-8 -*-
import json
mp = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_id_map.json', encoding='utf-8'))
print('映射条数:', len(mp))
# 反向查找：value 是这些 noteId 的 key（_help_xxx）
targets = {'BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM','RC8UiNCM0fHL'}
if isinstance(mp, dict):
    for k,v in mp.items():
        if v in targets:
            print(k, '->', v)
    # 也看看 导入/导出/ETAPI/指标 相关的键
    for k,v in mp.items():
        if any(s in k for s in ['Import','import','Export','export','ETAPI','Metric','metric','File','_file']):
            print('  candidate:', k, '->', v)
