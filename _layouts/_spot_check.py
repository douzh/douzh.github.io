# -*- coding: utf-8 -*-
import json, re
d = json.load(open('_extracted.json', encoding='utf-8'))
for nid in ['3GTPwnM0LXzq','sPaAXjVcvZZl','o5byGpiYwkAf']:
    c = d['content'][nid]
    print('='*100)
    print('### %s len=%d' % (nid, len(c)))
    print(c[:2500])
