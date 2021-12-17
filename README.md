To reproduce memory leak on `win32` systems:

* Install Erlang 23.1.5 and 23.2 to the default `C:\Program Files` location.
* Run this command. It will not reproduce the leak in any case:
    ```
    .\run.ps1 -erlang_version '23.1.5' -leak $false
    .\run.ps1 -erlang_version '23.1.5' -leak $true
    ```
* Run this command. It will reproduce the leak:
    ```
    .\run.ps1 -erlang_version '23.2' -leak $true
    ```
* Run this command. It will *NOT* reproduce the leak:
    ```
    .\run.ps1 -erlang_version '23.2' -leak $false
    ```

To see the leak in action, start one of the above commands. While the command
runs, start the "Performance Monitor" application as an administrator. Click on
`Performance Monitor`, click on the default monitor (CPU usage) and click the
red `X` to remove it. Click the green `+`. Choose the `Process - Private Bytes`
counter, and in `Instances of selected object:` choose `erl`. Click `Add` then
`OK`.

Right-click `Performance Monitor` and select `Properties`. Use these settings:

* `General` tab - "Sample every" 1 seconds, Duration 1000 seconds
* `Graph` tab - select "Horizontal grid". "Vertical scale" minimum can be `0`, maximum of `3000` should be sufficient

You should now see a red line graph being drawn showing "Private Bytes" usage
for the `erl.exe` process. During the leak scenarios, it will go up quickly.
