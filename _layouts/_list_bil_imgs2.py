# -*- coding: utf-8 -*-
import sqlite3, re, json
db=r'F:\trilium\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

rows=cur.execute("""
SELECT n.noteId, n.title, n.blobId
FROM notes n
WHERE n.isDeleted=0 AND n.noteId NOT LIKE '_help%' AND n.noteId NOT LIKE '_template%' AND n.noteId NOT LIKE '_lb%'
AND n.title LIKE '%(%' AND n.type='text'
""").fetchall()

result=[]
for nid,title,bid in rows:
    if not bid or bid=='z4PhNX7vuL3xVChQ1m2A':
        continue
    c=cur.execute("SELECT content FROM blobs WHERE blobId=?", (bid,)).fetchone()
    if not c or not c[0]:
        continue
    txt=c[0]
    imgs=re.findall(r'<img[^>]*src="([^"]+)"', txt)
    rel=[x for x in imgs if not x.startswith('http') and not x.startswith('api/') and not x.startswith('#root') and not x.startswith('data:')]
    if rel:
        result.append({'noteId':nid,'title':title,'imgs':rel,'full':txt})

json.dump(result, open('_bil_imgs.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
total=sum(len(r['imgs']) for r in result)
print('notes with rel images:', len(result))
print('total rel imgs:', total)
print('sample:', json.dumps(result[:3], ensure_ascii=False, indent=1)[:800])
