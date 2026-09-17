# -*- coding: utf-8 -*-
"""从双语 HTML 中提取中文：保留每个文本块中第一个中文字符之后的内容，删除无中文的文本块。
用法: python _extract_cn2.py <noteId>"""
import sys, re, sqlite3
from html.parser import HTMLParser

BLOCK_TAGS = {'p','li','h1','h2','h3','h4','h5','h6','td','th','dt','dd','figcaption','caption','blockquote'}
KEEP_RAW = {'pre','code'}   # 代码块原样保留

class Node:
    __slots__ = ('tag','attrs','children','text')
    def __init__(self, tag=None, attrs=None, text=None):
        self.tag = tag
        self.attrs = attrs or []
        self.children = []
        self.text = text

class TreeBuilder(HTMLParser):
    VOID = {'br','img','hr','wbr','input','meta','link','area','base','col','embed','source','track'}
    def __init__(self):
        super().__init__(convert_charrefs=False)
        self.root = Node('__root__')
        self.stack = [self.root]
    def handle_starttag(self, tag, attrs):
        n = Node(tag, attrs)
        self.stack[-1].children.append(n)
        if tag not in self.VOID:
            self.stack.append(n)
    def handle_startendtag(self, tag, attrs):
        n = Node(tag, attrs)
        self.stack[-1].children.append(n)
    def handle_endtag(self, tag):
        for i in range(len(self.stack)-1, 0, -1):
            if self.stack[i].tag == tag:
                del self.stack[i:]
                break
    def handle_data(self, data):
        if self.stack:
            self.stack[-1].children.append(Node(text=data))
    def handle_entityref(self, name):
        self.handle_data('&%s;' % name)
    def handle_charref(self, name):
        self.handle_data('&#%s;' % name)

CN_RE = re.compile(r'[\u4e00-\u9fff]')

def has_cn(s):
    return bool(CN_RE.search(s))

def node_text(n):
    if n.text is not None:
        return n.text
    return ''.join(node_text(c) for c in n.children)

def serialize(n):
    if n.text is not None:
        return n.text
    attrs = ''
    for k, v in n.attrs:
        if v is None:
            attrs += ' %s' % k
        else:
            attrs += ' %s="%s"' % (k, v.replace('"','&quot;'))
    inner = ''.join(serialize(c) for c in n.children)
    return '<%s%s>%s</%s>' % (n.tag, attrs, inner, n.tag)

def clean_empty(node):
    """清理空标签与多余嵌套（保留有属性的空标签如 img）。"""
    if node.text is not None:
        return node
    # 递归清理子节点
    kept = []
    for c in node.children:
        if c.text is not None:
            kept.append(c)
            continue
        c = clean_empty(c)
        # 丢弃空的结构性标签（无文本、无子元素、无必要属性）
        if not c.children and c.tag not in ('img','br','hr','input','meta','link'):
            continue
        kept.append(c)
    node.children = kept
    return node

def process(node):
    """就地处理，返回保留下来的节点列表（通常 [node] 或若干子节点）。"""
    if node.text is not None:
        return [node]
    if node.tag in KEEP_RAW:
        return [node]
    if node.tag in BLOCK_TAGS:
        return process_block(node)
    # 结构性标签：递归处理子节点，自身保留
    kept = []
    for c in node.children:
        kept.extend(process(c))
    node.children = kept
    return [node]

# "英文(中文)" 格式链接文本（如 Options(选项)），不贡献切割点
EN_CN_PAIR_RE = re.compile(r'^\s*[A-Za-z][^()]*\([^()]*[\u4e00-\u9fff][^()]*\)\s*$')

def cut_block(children):
    """在 children 列表中找到第一个中文字符，返回切割后的节点列表；无中文返回 None。"""
    for i, c in enumerate(children):
        if c.text is not None:
            m = CN_RE.search(c.text)
            if m:
                kept = []
                head = c.text[m.start():]
                if head.strip():
                    kept.append(Node(text=head))
                kept.extend(children[i+1:])
                return kept
            continue
        txt = node_text(c)
        if EN_CN_PAIR_RE.match(txt):
            # 链接文本本身是"英文(中文)"格式，跳过（不贡献切割点）
            continue
        if has_cn(txt):
            newc = cut_element(c)
            if newc is not None:
                return [newc] + list(children[i+1:])
            continue
    return None

def cut_element(elem):
    """对元素节点递归切割到第一个中文字符，返回新元素；无中文返回 None。"""
    if elem.text is not None:
        m = CN_RE.search(elem.text)
        if m:
            return Node(text=elem.text[m.start():])
        return None
    kept = cut_block(elem.children)
    if kept is None:
        return None
    new = Node(elem.tag, list(elem.attrs))
    new.children = kept
    return new

def process_block(block):
    """处理文本块：截断到第一个中文字符；无中文则删除。返回节点列表。"""
    # 递归处理内部子元素（嵌套 li/ul/table 等）
    inner = []
    for c in block.children:
        inner.extend(process(c))
    block.children = inner

    cut = cut_block(block.children)
    if cut is None:
        # 无中文：若块内含结构化内容（嵌套 ul/ol/table，其中已有中文），保留结构，否则删除整块
        structs = [c for c in block.children if c.tag is not None and c.tag in ('ul','ol','table')]
        return structs
    block.children = cut
    return [block]

def extract_cn(html):
    tb = TreeBuilder()
    try:
        tb.feed(html)
        tb.close()
    except Exception as e:
        return None, str(e)
    kept = process(tb.root)
    clean_empty(tb.root)
    result = ''.join(serialize(n) for n in tb.root.children)
    return result, None

if __name__ == '__main__':
    nid = sys.argv[1]
    DB = r'F:\trilium\trilium-data\document.db'
    conn = sqlite3.connect(DB)
    conn.text_factory = lambda b: b.decode('utf-8', 'replace')
    cur = conn.cursor()
    row = cur.execute("SELECT title, blobId FROM notes WHERE noteId=?", (nid,)).fetchone()
    c = cur.execute("SELECT content FROM blobs WHERE blobId=?", (row[1],)).fetchone()[0]
    conn.close()
    print('原文 len=%d' % len(c))
    res, err = extract_cn(c)
    if err:
        print('ERROR:', err)
        sys.exit(1)
    print('结果 len=%d' % len(res))
    print('='*90)
    print(res)
