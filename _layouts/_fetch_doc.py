# -*- coding: utf-8 -*-
import urllib.request, re

url='https://docs.triliumnotes.org/user-guide/setup/desktop/tray-icon-and-startup'
try:
    req=urllib.request.Request(url, headers={'User-Agent':'Mozilla/5.0'})
    with urllib.request.urlopen(req, timeout=20) as r:
        body=r.read().decode('utf-8','replace')
    print('len:', len(body))
    for m in re.finditer(r'<img[^>]*src="([^"]+)"', body):
        print('IMG:', m.group(1))
    # markdown links to images
    for m in re.finditer(r'!\[[^\]]*\]\(([^)]+)\)', body):
        print('MDIMG:', m.group(1))
except Exception as e:
    print('ERR', e)
