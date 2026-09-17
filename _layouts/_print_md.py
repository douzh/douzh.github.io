# -*- coding: utf-8 -*-
import sys
fn = r'D:\mycloud\0pnbase\kb-main\_layouts\_leaf_src\%s.md' % sys.argv[1]
c = open(fn, encoding='utf-8').read()
sys.stdout.buffer.write(c.encode('utf-8'))
