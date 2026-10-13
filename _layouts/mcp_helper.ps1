param(
  [Parameter(Mandatory=$true)][string]$Name,
  [Parameter(Mandatory=$true)][string]$ArgumentsJson
)
$argsObj = $ArgumentsJson | ConvertFrom-Json
$body = @{ jsonrpc='2.0'; id=[string](Get-Random); method='tools/call'; params=@{ name=$Name; arguments=$argsObj } } | ConvertTo-Json -Depth 20
$tmp = "$env:TEMP\mcp_call.json"
[IO.File]::WriteAllText($tmp, $body, (New-Object System.Text.UTF8Encoding($false)))
$resp = curl.exe -s -X POST http://127.0.0.1:37840/mcp -H "Authorization: dErucsr17Ybr_dbnEu20jKX4b48O8USXrzDlWbhfhCSyvyYYR60KpGgE=" -H "Content-Type: application/json" -H "Accept: application/json, text/event-stream" --data-binary "@$tmp"
$lines = $resp -split "`n"
$dataLine = ($lines | Where-Object { $_ -like 'data:*' } | Select-Object -Last 1)
$json = $dataLine -replace '^data:\s?',''
try {
  $obj = $json | ConvertFrom-Json
  return $obj.result.content[0].text
} catch {
  return "PARSE_ERROR resp=$resp"
}
