# -*- coding: utf-8 -*-
import json
fp=json.load(open('_fixplan.json',encoding='utf-8'))
print('fixplan structure:', type(fp), len(fp))
# find structure
if isinstance(fp, dict):
    keys=list(fp.keys())[:3]
    for k in keys[:2]:
        print('note:', k)
        print(json.dumps(fp[k], ensure_ascii=False, indent=1)[:1500])
elif isinstance(fp, list):
    print(json.dumps(fp[0], ensure_ascii=False, indent=1)[:1500])
