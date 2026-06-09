<#
.SYNOPSIS
    Builds the AMDP Pretty Printer Eclipse plugin.

.DESCRIPTION
    Generates the machine-specific .target file from the template in src/target/,
    substituting the local Eclipse installation path, then runs "mvn package".

.PARAMETER EclipseHome
    Path to the Eclipse installation directory (must have SAP ADT installed).
    Defaults to C:/eclipse/eclipse.

.EXAMPLE
    .\build-plugin.ps1
    .\build-plugin.ps1 -EclipseHome "D:/tools/eclipse"
#>
param(
    [string]$EclipseHome = "C:/eclipse/eclipse"
)

$ErrorActionPreference = "Stop"

$template = Join-Path $PSScriptRoot "src\target\amdp-pretty-printer-eclipse-plugin.target"
$output   = Join-Path $PSScriptRoot "amdp-pretty-printer-eclipse-plugin.target"

Write-Host "Generating target platform file for Eclipse at: $EclipseHome"

$escapedPath = $EclipseHome -replace '\\', '/'
$content = Get-Content $template -Raw
$content = $content -replace '\$\{eclipse\.install\.dir\}', $escapedPath
Set-Content -Path $output -Value $content -Encoding UTF8 -NoNewline

Write-Host "Written: $output"
Write-Host ""
Write-Host "Running: mvn package"
mvn package
