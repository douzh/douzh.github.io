# -*- coding: utf-8 -*-
import urllib.request

def try_url(url, extra_headers=None):
    try:
        req=urllib.request.Request(url, headers={'User-Agent':'Mozilla/5.0'})
        if extra_headers:
            for k,v in extra_headers.items():
                req.add_header(k,v)
        with urllib.request.urlopen(req, timeout=10) as r:
            body=r.read(200)
            print(url)
            print('  status:', r.status, 'ct:', r.headers.get('Content-Type'), 'len:', r.headers.get('Content-Length'))
            print('  head:', body[:60])
    except Exception as e:
        print(url, '-> ERR', str(e)[:120])

base='http://127.0.0.1:37840'
# try doc_notes static
try_url(base+'/assets/v0.105.0/doc_notes/en/User%20Guide/User%20Guide/Installation%20%26%20Setup/Desktop%20Installation/Tray%20icon%20%26%20automatic%20startup_image.png')
try_url(base+'/assets/v0.105.0/doc_notes/en/User%20Guide/User%20Guide/Installation%20%26%20Setup/Desktop%20Installation/Tray%20icon%20%26%20automatic%20startup.html')
# root
try_url(base+'/')
# MCP endpoint
try_url(base+'/mcp')
