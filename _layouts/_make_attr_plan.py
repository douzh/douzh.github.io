# -*- coding: utf-8 -*-
import json
mapping=json.load(open('_bil_mapping.json',encoding='utf-8'))
print('mapping entries:', len(mapping))
# unique docNames
docnames=set(m['docName'] for m in mapping)
print('unique docNames:', len(docnames))
# check for missing
missing=[m for m in mapping if not m.get('docName')]
print('missing docName:', len(missing))
for m in mapping[:5]:
    print(' ', m['noteId'], '|', m['title'], '|', m['docName'])
# save simple list: noteId -> docName
plan=[{'noteId':m['noteId'],'title':m['title'],'docName':m['docName']} for m in mapping]
json.dump(plan, open('_attr_plan.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print()
print('saved attr plan with', len(plan), 'entries')
