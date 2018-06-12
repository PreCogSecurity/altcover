Import-Module "./_Packaging/Module/tools/netcoreapp2.0/AltCover.PowerShell.dll"
Import-Module "./packages/Pester.4.3.1/tools/Pester.psm1"

Invoke-Altcover -?
Invoke-Pester -Script .\Build -EnableExit -OutputFormat NUnitXml -OutputFile "./_Reports/PesterReport.xml"