SET TOOL_PATH=.paket

IF NOT EXIST "%TOOL_PATH%\paket.exe" (
  dotnet tool install paket --tool-path ./%TOOL_PATH%
)

"%TOOL_PATH%/paket.exe" restore

SET TOOL_PATH=.fake

IF NOT EXIST "%TOOL_PATH%\fake.exe" (
  dotnet tool install fake-cli --tool-path ./%TOOL_PATH%
)

"%TOOL_PATH%/fake.exe" %*