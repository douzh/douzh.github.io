# -*- coding: utf-8 -*-
"""填充 Navigation 批1：Tree Concepts + One Pane Layout + Bookmarks + Hoisting + Search in note + Similar Notes"""
import json, urllib.request

URL = "http://127.0.0.1:37840/mcp"
HEADERS = {
    "Authorization": "dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=",
    "Accept": "application/json, text/event-stream",
    "Content-Type": "application/json",
}
MAP = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\help_map.json', encoding='utf-8'))['node_map']

def set_content(nid, content, rid):
    payload = {"jsonrpc": "2.0", "id": rid, "method": "tools/call", "params": {
        "name": "set_note_content", "arguments": {"noteId": nid, "content": content}}}
    body = json.dumps(payload, ensure_ascii=False).encode("utf-8")
    req = urllib.request.Request(URL, data=body, headers=HEADERS, method="POST")
    with urllib.request.urlopen(req, timeout=30) as resp:
        return resp.read().decode("utf-8", errors="replace")[:60].replace("\n", " ")

pages = {}

pages['_help_kBrnXNG3Hplm'] = """This page explains the basic concepts related to the tree structure of notes in TriliumNext.
此页面解释 TriliumNext 中与笔记树结构相关的基本概念。

## Note / 笔记

A note is the central entity in TriliumNext. For more details, see <a class="reference-link" href="#root/_help_BFs8mudNFgCS">Notes</a>.
笔记是 TriliumNext 中的核心实体。更多详情，参见<a class="reference-link" href="#root/_help_BFs8mudNFgCS">笔记（Notes）</a>。

## Branch / 分支

A branch describes the placement of a note within the note tree. Essentially, it is a tuple of `parentNoteId` and `noteId`, indicating that the given note is placed as a child under the specified parent note.
分支（branch）描述笔记在笔记树中的位置。本质上，它是 `parentNoteId` 和 `noteId` 的元组，表示给定笔记被放置在指定父笔记之下作为其子笔记。

Each note can have multiple branches, meaning any note can be placed in multiple locations within the tree. This concept is referred to as <a class="reference-link" href="#root/_help_IakOLONlIfGI">Cloning Notes</a>.
每个笔记可以有多个分支，意味着任何笔记都可以被放置在树中的多个位置。这个概念被称为<a class="reference-link" href="#root/_help_IakOLONlIfGI">克隆笔记（Cloning Notes）</a>。

## Prefix / 前缀

A prefix is a branch-specific title modifier for a note. If you place your note in two different locations within the tree and want to alter the title slightly in one of those placements, you can use a prefix.
前缀（prefix）是笔记特定于分支的标题修饰符。如果你将笔记放置在树中的两个不同位置，并想在其中一个位置稍微改变标题，可以使用前缀。

To edit a prefix, right-click on the note in the tree pane and select _Edit branch prefix_.
要编辑前缀，右键点击树窗格中的笔记并选择 _编辑分支前缀（Edit branch prefix）_。

The prefix is not part of the note itself and is not encrypted when the note is protected. This can be useful if you want part of the title to remain visible in the tree for easier navigation, even when the note is protected.
前缀不是笔记本身的一部分，并且当笔记受保护时不会被加密。如果你希望部分标题即使在笔记受保护时也能在树中保持可见以便于导航，这会很有用。

## Subtree / 子树

A subtree consists of a particular note (the subtree root) and all its children and descendants. Some operations, such as exporting, work on entire subtrees.
子树（subtree）由特定笔记（子树根）及其所有子笔记和后代组成。某些操作（如导出）作用于整个子树。"""

pages['_help_MMiBEQljMQh2'] = """One of the Trilium's goals is to provide fast and comfortable navigation between notes.
Trilium 的目标之一是提供笔记之间快速舒适的导航。

## Backwards and forward / 后退和前进

You can use alt-left and alt-right to move back and forward in history of viewed pages.
你可以使用 alt-left 和 alt-right 在已查看页面的历史记录中后退和前进。

This works identically to browser backwards / forwards, it's actually using built-in browser support for this.
这与浏览器的后退/前进完全相同，实际上它使用了浏览器内置的支持。

![](Note%20Navigation_image.png)

## Jump to note / 跳转到笔记

This is useful to quickly find and view arbitrary notes - click on `Jump to`button on the top or press <kbd>Ctrl</kbd> + <kbd>J</kbd> . Then type part of the note name and autocomplete will help you pick the desired note.
这对于快速查找和查看任意笔记很有用——点击顶部的 `跳转到（Jump to）`按钮或按 <kbd>Ctrl</kbd> + <kbd>J</kbd>。然后输入部分笔记名称，自动补全将帮助你选择所需的笔记。

See <a class="reference-link" href="#root/_help_F1r9QtzQLZqm">Jump to Note</a> for more information.
更多信息参见<a class="reference-link" href="#root/_help_F1r9QtzQLZqm">跳转到笔记（Jump to Note）</a>。"""

pages['_help_u3YFHC9tQlpm'] = """Frequently used notes can be bookmarked, which will make them appear in the <a class="reference-link" href="#root/_help_xYmIYSP6wE3F">Launch Bar</a> for easy access.
经常使用的笔记可以被收藏（bookmarked），这将使它们出现在<a class="reference-link" href="#root/_help_xYmIYSP6wE3F">启动栏（Launch Bar）</a>中，方便访问。

## Configuring the launch bar / 配置启动栏

If bookmarks don't appear in the launch bar, then most likely the bookmark section has been hidden. Go to the <a class="reference-link" href="#root/_help_xYmIYSP6wE3F">Launch Bar</a> configuration from the <a class="reference-link" href="#root/_help_x3i7MxGccDuM">Global menu</a> and ensure _Bookmarks_ is in the _Visible Launchers_ section.
如果收藏没有出现在启动栏中，那么很可能是收藏部分被隐藏了。从<a class="reference-link" href="#root/_help_x3i7MxGccDuM">全局菜单（Global menu）</a>进入<a class="reference-link" href="#root/_help_xYmIYSP6wE3F">启动栏</a>配置，确保 _收藏（Bookmarks）_ 在 _可见启动器（Visible Launchers）_ 部分中。

## Bookmark folder / 收藏文件夹

Space in the left panel is limited, and you might want to bookmark many items. One possible solution is to bookmark a folder, so it shows its children:
左侧面板的空间有限，你可能想收藏许多项目。一种可能的解决方案是收藏一个文件夹，这样它会显示其子笔记：

To do this, bookmark a folder and assign it the `#bookmarkFolder` label.
要做到这一点，收藏一个文件夹并为其分配 `#bookmarkFolder` 标签。

## Mobile / 移动端

On mobile, bookmarks are only displayed starting with v0.102.0. Because of the more constrained screen size, the bookmarks are grouped under a single icon instead of displaying them as separate icons.
在移动端，收藏从 v0.102.0 开始才显示。由于屏幕尺寸更受限，收藏被分组在单个图标下，而不是显示为单独的图标。

When pressed, a menu will appear listing all the bookmarks. Bookmark folders are also supported and will appear as sub-menus.
按下后，将出现一个列出所有收藏的菜单。收藏文件夹也受支持，并将显示为子菜单。"""

pages['_help_OR8WJ7Iz9K4U'] = """Hoisting is a standard outliner feature which allows you to focus on (or "zoom into") a specific note and its subtree by hiding all parent and sibling notes. Demo:
提升（hoisting）是一个标准的提纲（outliner）功能，允许你通过隐藏所有父笔记和同级笔记来专注于（或"放大到"）特定笔记及其子树。演示：

![](Note%20Hoisting_note-hoisting.gif)

In addition to showing only this subtree, this also narrows both full text search and ["jump to note"](#root/_help_MMiBEQljMQh2) to just notes present in hoisted subtree.
除了只显示此子树外，这还将全文搜索和["跳转到笔记"](#root/_help_MMiBEQljMQh2)都收窄到仅限提升子树中存在的笔记。

See also [Workspace](#root/_help_9sRHySam5fXb) which extends this feature.
另见扩展此功能的[工作区（Workspace）](#root/_help_9sRHySam5fXb)。"""

pages['_help_McngOG2jbUWX'] = """<figure class="image image_resized" style="width:100%;"><img style="aspect-ratio:898/93;" src="Search in note_image.png" width="898" height="93"></figure>

Local search allows you to search within the currently displayed note.
本地搜索（local search）允许你在当前显示的笔记内进行搜索。

## Alternatives / 备选方式

*   Pressing Ctrl+F while in a browser while not focused in a <a class="reference-link" href="#root/_help_iPIMuisry3hd">Text</a> or a <a class="reference-link" href="#root/_help_6f9hih2hXXZk">Code</a> note will trigger the browser's native search. This will also find text that is part of Trilium's UI.
*   Pressing Ctrl+F in a <a class="reference-link" href="#root/_help_iPIMuisry3hd">Text</a> note will reveal <a class="reference-link" href="#root/_help_MI26XDLSAlCD">CKEditor</a>'s search functionality.
*   在浏览器中当焦点不在<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>或<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记中时按 Ctrl+F 将触发浏览器的原生搜索。这也会找到属于 Trilium UI 一部分的文本。
*   在<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本</a>笔记中按 Ctrl+F 将显示<a class="reference-link" href="#root/_help_MI26XDLSAlCD">CKEditor</a>的搜索功能。

## Accessing the search / 访问搜索

*   On desktop, press<kbd>Ctrl</kbd> + <kbd>F</kbd>
*   From the <a class="reference-link" href="#root/_help_8YBEPzcpUgxw">Note buttons</a>, look for the context menu and select _Search in note_.
*   在桌面上，按 <kbd>Ctrl</kbd> + <kbd>F</kbd>
*   从<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a>中，查找上下文菜单并选择 _在笔记中搜索（Search in note）_。

## Interaction / 交互

*   Finding / 查找：
    *   Fill in the _Find in text\u2026_ with the text to search for.
    *   The search will be executed automatically in the background.
    *   Use up and down arrows of the text box to navigate between results.
    *   在 _在文本中查找\u2026（Find in text\u2026）_ 中填写要搜索的文本。
    *   搜索将在后台自动执行。
    *   使用文本框的上、下箭头在结果之间导航。
*   Replacing / 替换：
    *   Fill in the _Find in text_… field with the text to replace.
    *   Fill in the _Replace with\u2026_ field the text to replace it with.
    *   Press _Replace_ to replace only the current result.
    *   Press _Replace all_ to replace all of them at once.
    *   在 _在文本中查找（Find in text）_\u2026 字段中填写要替换的文本。
    *   在 _替换为\u2026（Replace with\u2026）_ 字段中填写用于替换的文本。
    *   按 _替换（Replace）_ 只替换当前结果。
    *   按 _全部替换（Replace all）_ 一次性替换所有结果。
*   Options / 选项：
    *   _Case sensitive_ \u2013 the search will distinguish upper case characters from lower case (e.g. searching for Hello will not match `hello`).
    *   _Match words_ - the search will find only exact word matches (e.g. searching for `Java` will not match `JavaScript`).
    *   _区分大小写（Case sensitive）_ \u2013 搜索将区分大写和小写字符（例如搜索 Hello 不会匹配 `hello`）。
    *   _全词匹配（Match words）_ - 搜索将只找到完全匹配的单词（例如搜索 `Java` 不会匹配 `JavaScript`）。"""

pages['_help_xWtq5NUHOwql'] = """<figure class="image image-style-align-right image_resized" style="width:50.46%;"><img style="aspect-ratio:538/290;" src="Similar Notes_image.png" width="538" height="290"></figure>

The Similar Notes feature tries to identify notes that relate to the current note by looking at the content of the notes, their relationships, as well as the date they were created.
相似笔记（Similar Notes）功能尝试通过查看笔记的内容、它们的关系以及创建日期来识别与当前笔记相关的笔记。

To access the list of similar notes:
要访问相似笔记列表：

*   On the <a class="reference-link" href="#root/_help_IjZS7iK5EXtb">New Layout</a>, go to the <a class="reference-link" href="#root/_help_KRlZFoIH0j6p">Connections tab</a> in the <a class="reference-link" href="#root/_help_RnaPdbciOfeq">Right Sidebar</a> and look for the corresponding section.
*   On the old layout, press the _Similar Notes_ tab in the <a class="reference-link" href="#root/_help_BlN9DFI679QC">Ribbon</a>.
*   在<a class="reference-link" href="#root/_help_IjZS7iK5EXtb">新布局（New Layout）</a>上，进入<a class="reference-link" href="#root/_help_RnaPdbciOfeq">右侧边栏（Right Sidebar）</a>中的<a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接标签页（Connections tab）</a>并查找相应部分。
*   在旧布局上，按下<a class="reference-link" href="#root/_help_BlN9DFI679QC">功能区（Ribbon）</a>中的 _相似笔记（Similar Notes）_ 标签页。

## Interaction / 交互

*   Hover over a note to see a short preview of the note.
*   Click over a note to open it in the current view.
*   <kbd>Ctrl</kbd> + click a note to open it in a separate tab.
*   悬停在笔记上以查看笔记的简短预览。
*   点击笔记在当前视图中打开它。
*   <kbd>Ctrl</kbd> + 点击笔记在单独的标签页中打开它。"""

rid = 7400
for src, content in pages.items():
    nid = MAP.get(src)
    if not nid:
        print("NO-MAP", src)
        continue
    r = set_content(nid, content, rid)
    rid += 1
    print("WRITTEN", src, "->", nid, r)
