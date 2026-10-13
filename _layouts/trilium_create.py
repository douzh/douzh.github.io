# -*- coding: utf-8 -*-
# Read a body file, create a Trilium text note under parentNoteId, then read it back.
# usage: python trilium_create.py <body_file> <title> [parentNoteId]
import sys, json, urllib.request

MCP = "http://127.0.0.1:37840/mcp"
AUTH = "dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE="

def call_tool(name, arguments):
    payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {"name": name, "arguments": arguments},
    }
    data = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(MCP, data=data, method="POST")
    req.add_header("Authorization", AUTH)
    req.add_header("Content-Type", "application/json")
    req.add_header("Accept", "application/json, text/event-stream")
    with urllib.request.urlopen(req, timeout=60) as r:
        raw = r.read().decode("utf-8")
    # parse SSE: take last line starting with data:
    txt = raw
    for line in raw.splitlines():
        if line.startswith("data:"):
            txt = line[5:].strip()
    obj = json.loads(txt)
    return obj["result"]["content"][0]["text"]

def main():
    body_file = sys.argv[1]
    title = sys.argv[2]
    parent = sys.argv[3] if len(sys.argv) > 3 else "kGWea1U3uJHh"
    with open(body_file, "r", encoding="utf-8") as f:
        content = f.read()
    res = call_tool("create_note", {"parentNoteId": parent, "title": title,
                                    "type": "text", "content": content})
    print("=== CREATE RESULT ===")
    print(res)
    # extract noteId
    try:
        rj = json.loads(res)
        note_id = rj.get("noteId") or rj.get("note", {}).get("noteId")
    except Exception:
        note_id = None
    if not note_id:
        # try regex
        import re
        m = re.search(r'"noteId"\s*:\s*"([^"]+)"', res)
        note_id = m.group(1) if m else None
    print("=== NOTE_ID =", note_id)
    if note_id:
        back = call_tool("get_note_content", {"noteId": note_id})
        try:
            bj = json.loads(back)
            c = bj.get("content", "")
            print("=== READBACK title-noteId:", bj.get("noteId"))
            print("=== READBACK content length:", len(c))
            print("=== READBACK head:", c[:80].replace("\n", " "))
        except Exception:
            print("=== READBACK RAW:", back[:300])

if __name__ == "__main__":
    main()
