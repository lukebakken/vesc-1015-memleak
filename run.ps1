param($erlang_version='24.2')

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 'Latest' -ErrorAction 'Stop'  -Verbose

& "C:\tools\erl-$erlang_version\bin\erlc.exe" +debug repro.erl

# & werl.exe -boot "start_sasl" +W w +MBas ageffcbf +MHas ageffcbf +MBlmbcs 512 +MHlmbcs 512 +MMmcs 30 +P 1048576 +t 5000000 +stbt db +zdbbl 128000 +sbwt none +sbwtdcpu none +sbwtdio none

& "C:\tools\erl-$erlang_version\bin\werl.exe"
