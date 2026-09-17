# -*- coding: utf-8 -*-
import html2text
h2t=html2text.HTML2Text()
h2t.body_width=0
h2t.ignore_images=False
h2t.ignore_links=False
h2t.unicode_snob=True
s='<p>The default note type is <a class="reference-link" href="#root/B5oluBvu2gBY">Text</a>, ok.</p><p>See <a href="https://example.com">ext</a>.</p>'
print(repr(h2t.handle(s)))
