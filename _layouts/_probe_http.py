# -*- coding: utf-8 -*-
import urllib.request, json

# Probe nearby ports for Trilium ETAPI / HTTP
for port in [37740, 37840, 38080, 8080, 3770, 37770]:
    url='http://127.0.0.1:%d/' % port
    try:
        req=urllib.request.Request(url, timeout=3)
        with urllib.request.urlopen(req) as r:
            body=r.read()[:200].decode('utf-8','replace')
            print(port, r.status, body[:120].replace('\n',' '))
    except Exception as e:
        print(port, 'ERR', str(e)[:100])
