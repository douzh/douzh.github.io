# -*- coding: utf-8 -*-
import sqlite3
db=r'C:\Users\douzh\AppData\Roaming\trilium-data\document.db'
con=sqlite3.connect('file:'+db+'?mode=ro', uri=True)
cur=con.cursor()

# 1. ALL attachments including deleted
rows=cur.execute("SELECT attachmentId, ownerId, role, mime, title, isDeleted FROM attachments").fetchall()
print('=== all attachments (%d) ===' % len(rows))
for r in rows:
    print('  ', r)

# 2. blobs: count by content type / check for image magic bytes
rows=cur.execute("SELECT blobId, length(content) FROM blobs").fetchall()
print('\n=== blobs total:', len(rows))
img_blobs=[]
for blobId, ln in rows:
    if ln>0:
        c=cur.execute("SELECT content FROM blobs WHERE blobId=?", (blobId,)).fetchone()[0]
        if c[:8]==b'\x89PNG\r\n\x1a\n' or c[:3]==b'\xff\xd8\xff' or (len(c)>4 and c[:4]==b'RIFF'):
            img_blobs.append((blobId, ln, c[:16]))
print('image blobs (png/jpg/webp):', len(img_blobs))
for b in img_blobs[:30]:
    print('  ', b)

# 3. notes that reference these blobs (image notes)
print('\n=== image/file notes ===')
rows=cur.execute("SELECT noteId, title, type, mime, blobId FROM notes WHERE blobId != 'z4PhNX7vuL3xVChQ1m2A' AND blobId IS NOT NULL AND blobId != ''").fetchall()
print('notes with real blobs:', len(rows))
for r in rows[:40]:
    print('  ', r)
