SET TOOL_PATH=.tool

IF NOT EXIST "%TOOL_PATH%\paket.exe" (
  dotnet tool install paket --tool-path ./%TOOL_PATH%  --version 5.207.0
)

"%TOOL_PATH%/paket.exe" restore

IF NOT EXIST "%TOOL_PATH%\fake.exe" (
  dotnet tool install fake-cli --tool-path ./%TOOL_PATH% --version 5.13.7
)

"%TOOL_PATH%/fake.exe" %*