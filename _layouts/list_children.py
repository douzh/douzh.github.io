import json, urllib.request

URL = "http://127.0.0.1:37840/mcp"
HEADERS = {
    "Authorization": "dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=",
    "Content-Type": "application/json",
    "Accept": "application/json, text/event-stream",
}

def call(method, name, args):
    body = {"jsonrpc":"2.0","id":"1","method":"tools/call","params":{"name":name,"arguments":args}}
    req = urllib.request.Request(URL, data=json.dumps(body).encode("utf-8"), headers=HEADERS, method="POST")
    with urllib.request.urlopen(req, timeout=30) as r:
        raw = r.read().decode("utf-8")
    for line in raw.splitlines():
        if line.startswith("data:"):
            return json.loads(line[5:].strip())
    return json.loads(raw)

res = call("x","get_child_notes",{"noteId":"kGWea1U3uJHh"})
txt = res["result"]["content"][0]["text"]
data = json.loads(txt)
notes = data if isinstance(data, list) else data.get("notes", [])
print("CHILD COUNT:", len(notes))
for n in notes:
    if isinstance(n, dict):
        print(n.get("noteId"), "|", n.get("title"))
    else:
        print(n)
