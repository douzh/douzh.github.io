# -*- coding: utf-8 -*-
import json
idmap = json.load(open('_id_map.json', encoding='utf-8'))
idmap['_help_Dgg7bR3b6K9j'] = 'KQaybwPDIrOm'
json.dump(idmap, open('_id_map.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('已补 _help_Dgg7bR3b6K9j -> KQaybwPDIrOm，共 %d 条' % len(idmap))
