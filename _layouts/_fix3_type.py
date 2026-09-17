# -*- coding: utf-8 -*-
import json, sqlite3, shutil, datetime
DB = r'F:\trilium\trilium-data\document.db'
ts = datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
bak = DB + '.bak_' + ts + '_book2text2'
shutil.copy2(DB, bak)
print('备份:', bak)

# 1) 补充 id_map 映射
mp = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\_id_map.json', encoding='utf-8'))
mp['_help_bOP3TB56fL1V'] = 'P5P0lLNUT9eC'  # grafana-dashboard.json
mp['_help_mHbBMPDPkVV5'] = 'BM2PpcgGjM0v'  # Import & Export -> 导入与导出
json.dump(mp, open(r'D:\mycloud\0pnbase\kb-main\_layouts\_id_map.json','w',encoding='utf-8'), ensure_ascii=False)
print('id_map 更新，条数:', len(mp))

# 2) 改类型 book->text（3 个）
conn = sqlite3.connect(DB)
cur = conn.cursor()
targets = ['BM2PpcgGjM0v','x6d4zSRRm1Id','6HcOzchIPSTM']
for nid in targets:
    cur.execute("UPDATE notes SET type='text' WHERE noteId=?", (nid,))
    print('改类型:', nid, 'rows=', cur.rowcount)
conn.commit()
conn.close()
print('完成')
