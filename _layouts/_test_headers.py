# -*- coding: utf-8 -*-
import urllib.request

url='http://127.0.0.1:37840/assets/v0.105.0/doc_notes/en/User%20Guide/User%20Guide/Installation%20%26%20Setup/Desktop%20Installation/Tray%20icon%20%26%20automatic%20startup_image.png'

def try_headers(label, hdrs):
    try:
        req=urllib.request.Request(url, headers=hdrs)
        with urllib.request.urlopen(req, timeout=10) as r:
            print(label, '->', r.status, r.headers.get('Content-Type'), 'len', r.headers.get('Content-Length'))
    except Exception as e:
        print(label, '-> ERR', str(e)[:90])

try_headers('plain', {'User-Agent':'Mozilla/5.0'})
try_headers('origin-self', {'User-Agent':'Mozilla/5.0','Origin':'http://127.0.0.1:37840'})
try_headers('referer-self', {'User-Agent':'Mozilla/5.0','Referer':'http://127.0.0.1:37840/'})
try_headers('host-header', {'User-Agent':'Mozilla/5.0','Host':'127.0.0.1:37840'})
try_headers('electron', {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Trilium/0.105.0 Chrome/126.0.0.0 Electron/31.0.0 Safari/537.36','Origin':'http://127.0.0.1:37840'})
