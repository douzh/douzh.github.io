# -*- coding: utf-8 -*-
import json, urllib.request

URL = 'http://127.0.0.1:37840/mcp'
HDR = {'Authorization': 'dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=', 'Content-Type': 'application/json', 'Accept': 'application/json, text/event-stream'}
_id = [0]


def call(tool, args):
    _id[0] += 1
    payload = {"jsonrpc": "2.0", "id": _id[0], "method": "tools/call",
               "params": {"name": tool, "arguments": args}}
    req = urllib.request.Request(URL, data=json.dumps(payload).encode('utf-8'),
                                 headers=HDR, method='POST')
    try:
        with urllib.request.urlopen(req, timeout=180) as r:
            data = r.read().decode('utf-8')
    except Exception as e:
        return 'ERR:' + str(e)
    out = []
    for line in data.splitlines():
        line = line.strip()
        if line.startswith('data:'):
            try:
                out.append(json.loads(line[5:].strip()))
            except Exception:
                pass
    if out:
        return json.dumps(out[-1])[:200]
    return data[:200]


def set_note(noteId, content):
    return call('set_note_content', {'noteId': noteId, 'content': content})


def get_note(noteId):
    r = call('get_note_content', {'noteId': noteId})
    return r
