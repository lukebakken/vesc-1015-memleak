param($erlang_version='23.2', $leak=$true, $erlang_install_base='C:\Program Files')

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 'Latest' -ErrorAction 'Stop'  -Verbose

& "$erlang_install_base\erl-$erlang_version\bin\erlc.exe" +debug .\repro.erl
& "$erlang_install_base\erl-$erlang_version\bin\erl.exe" -noinput -noshell -leak $leak -eval 'repro:run(1024), init:stop().'
