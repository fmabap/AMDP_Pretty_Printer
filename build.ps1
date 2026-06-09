<#
.SYNOPSIS
    Full build script for the AMDP Pretty Printer project.

.DESCRIPTION
    Performs the following steps:
      1. Build and install the core library into the local Maven repository.
      2. Build the CLI fat JAR.
      3. Build the Testcase Helper fat JAR.
      4. Build the Eclipse plugin (generates the .target file and runs mvn package).
      5. Create a native Windows executable for the CLI app via GraalVM native-image.
      6. Create a native Windows executable for the Testcase Helper via GraalVM native-image.
      7. Copy the resulting .exe files and the Eclipse plugin JAR into "artifacts".
      8. Copy the CLI fat JAR into the VS Code extension lib/ folder.
      9. Build the VS Code extension as a .vsix package.
     10. Copy the .vsix into "artifacts".

.PARAMETER EclipseHome
    Path to your Eclipse installation directory (must have SAP ADT installed).
    Defaults to C:/eclipse/eclipse.

.PARAMETER All
    If set, also creates native Windows executables via GraalVM native-image and copies them
    together with the Eclipse plugin JAR into "artifacts".

.EXAMPLE
    .\build.ps1
    .\build.ps1 -EclipseHome "D:/tools/eclipse"
    .\build.ps1 -All
    .\build.ps1 -EclipseHome "D:/tools/eclipse" -All
#>
param(
    [string]$EclipseHome = "C:/eclipse/eclipse",
    [switch]$All
)

$ErrorActionPreference = "Stop"
$RootDir = $PSScriptRoot
$JavaDir = Join-Path $RootDir "amdp-pretty-printer-java"
$VsCodeDir = Join-Path $RootDir "amdp-pretty-printer-vscode"
$Artifacts = Join-Path $RootDir "artifacts"

# ---------------------------------------------------------------------------
# Version constants – update here when bumping the project version
# ---------------------------------------------------------------------------
$Version = "2.0.0"
$EclipseVersion = "2.0.0"

function Step([string]$msg) {
    Write-Host ""
    Write-Host "=== $msg ===" -ForegroundColor Cyan
}

# ---------------------------------------------------------------------------
# Step 0 – Parent POM
# ---------------------------------------------------------------------------
Step "Step 0: Installing parent POM (mvn clean install -N)"
Set-Location $JavaDir
mvn clean install -N

# ---------------------------------------------------------------------------
# Step 1 – Core library
# ---------------------------------------------------------------------------
Step "Step 1: Building core library (mvn clean install)"
Set-Location (Join-Path $JavaDir "amdp-pretty-printer-core")
mvn clean install

# ---------------------------------------------------------------------------
# Step 2 – CLI application
# ---------------------------------------------------------------------------
Step "Step 2: Building CLI application (mvn clean package)"
Set-Location (Join-Path $JavaDir "amdp-pretty-printer-app")
mvn clean package

# ---------------------------------------------------------------------------
# Step 3 – Testcase Helper
# ---------------------------------------------------------------------------
Step "Step 3: Building Testcase Helper (mvn clean package)"
Set-Location (Join-Path $JavaDir "amdp-pretty-printer-testcase-helper")
mvn clean package

# ---------------------------------------------------------------------------
# Step 4 – Eclipse plugin
# ---------------------------------------------------------------------------
Step "Step 4: Building Eclipse plugin"
Set-Location (Join-Path $JavaDir "amdp-pretty-printer-eclipse-plugin")
.\build-plugin.ps1 -EclipseHome $EclipseHome

if ($All) {
    if (-not (Get-Command native-image -ErrorAction SilentlyContinue)) {
        Write-Error "native-image not found. Please install GraalVM and add it to your PATH."
        exit 1
    }

    # ---------------------------------------------------------------------------
    # Step 5 – Native executable for CLI app (GraalVM native-image)
    # ---------------------------------------------------------------------------
    Step "Step 5: Creating native Windows executable for CLI app (native-image)"
    Set-Location $RootDir
    native-image `
        -jar amdp-pretty-printer-java/amdp-pretty-printer-app/target/amdp-pretty-printer-app-$Version.jar `
        -o amdp-pretty-printer

    # ---------------------------------------------------------------------------
    # Step 6 – Native executable for Testcase Helper (GraalVM native-image)
    # ---------------------------------------------------------------------------
    Step "Step 6: Creating native Windows executable for Testcase Helper (native-image)"
    Set-Location $RootDir
    native-image `
        -jar amdp-pretty-printer-java/amdp-pretty-printer-testcase-helper/target/amdp-pretty-printer-testcase-helper-$Version.jar `
        -o amdp-pretty-printer-testcase-helper
}

# ---------------------------------------------------------------------------
# Step 7 – Copy artifacts to "artrifacts"
# ---------------------------------------------------------------------------
Step "Step 7: Copying artifacts to 'artrifacts'"

if (-not (Test-Path $Artifacts)) {
    New-Item -ItemType Directory -Path $Artifacts | Out-Null
}

if ($All) {
    # Native executable – CLI app (.exe on Windows)
    $ExePath = Join-Path $RootDir "amdp-pretty-printer.exe"
    Copy-Item -Path $ExePath -Destination $Artifacts -Force
    Write-Host "Copied: amdp-pretty-printer.exe  ->  $Artifacts"

    # Native executable – Testcase Helper (.exe on Windows)
    $TestcaseHelperExePath = Join-Path $RootDir "amdp-pretty-printer-testcase-helper.exe"
    Copy-Item -Path $TestcaseHelperExePath -Destination $Artifacts -Force
    Write-Host "Copied: amdp-pretty-printer-testcase-helper.exe  ->  $Artifacts"
}
else {
    Write-Host ""
    Write-Host "Skipped: native-image and artifact copy (use -All to enable)." -ForegroundColor Yellow
}

# Eclipse plugin JAR
$PluginJar = Join-Path $JavaDir `
    "amdp-pretty-printer-eclipse-plugin\target\com.github.fmabap.amdpprettyprinter.eclipse-$EclipseVersion.jar"
Copy-Item -Path $PluginJar -Destination $Artifacts -Force
Write-Host "Copied: com.github.fmabap.amdpprettyprinter.eclipse-$EclipseVersion.jar  ->  $Artifacts"

# CLI fat JAR
$AppJarDest = Join-Path $JavaDir "amdp-pretty-printer-app\target\amdp-pretty-printer-app-$Version.jar"
Copy-Item -Path $AppJarDest -Destination $Artifacts -Force
Write-Host "Copied: amdp-pretty-printer-app-$Version.jar  ->  $Artifacts"

# Testcase Helper fat JAR
$TestcaseHelperJar = Join-Path $JavaDir "amdp-pretty-printer-testcase-helper\target\amdp-pretty-printer-testcase-helper-$Version.jar"
Copy-Item -Path $TestcaseHelperJar -Destination $Artifacts -Force
Write-Host "Copied: amdp-pretty-printer-testcase-helper-$Version.jar  ->  $Artifacts"

# ---------------------------------------------------------------------------
# Step 8 – Copy CLI fat JAR into VS Code extension lib/
# ---------------------------------------------------------------------------
Step "Step 8: Copying CLI fat JAR to VS Code extension lib/"
$VsCodeLibDir = Join-Path $VsCodeDir "lib"
if (-not (Test-Path $VsCodeLibDir)) {
    New-Item -ItemType Directory -Path $VsCodeLibDir | Out-Null
}
$AppJar = Join-Path $JavaDir "amdp-pretty-printer-app\target\amdp-pretty-printer-app-$Version.jar"
Copy-Item -Path $AppJar -Destination (Join-Path $VsCodeLibDir "amdp-pretty-printer-app.jar") -Force
Write-Host "Copied: amdp-pretty-printer-app-$Version.jar  ->  $VsCodeLibDir\amdp-pretty-printer-app.jar"

# ---------------------------------------------------------------------------
# Step 9 – Build VS Code extension (.vsix)
# ---------------------------------------------------------------------------
Step "Step 9: Building VS Code extension (.vsix)"
# Copy root LICENSE so vsce does not prompt interactively
$RootLicense = Join-Path $RootDir "LICENSE"
if (Test-Path $RootLicense) {
    Copy-Item -Path $RootLicense -Destination $VsCodeDir -Force
    Write-Host "Copied: LICENSE  ->  $VsCodeDir"
}
Set-Location $VsCodeDir
npm run compile
$VsixPath = Join-Path $VsCodeDir "amdp-pretty-printer.vsix"
npx --yes @vscode/vsce package --out $VsixPath
Write-Host "Created: amdp-pretty-printer.vsix  ->  $VsCodeDir"

# Copy .vsix into "artrifacts"
if ($All) {
    if (-not (Test-Path $Artifacts)) {
        New-Item -ItemType Directory -Path $Artifacts | Out-Null
    }
    Copy-Item -Path $VsixPath -Destination $Artifacts -Force
    Write-Host "Copied: amdp-pretty-printer.vsix  ->  $Artifacts"
}
# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "=== Build complete! ===" -ForegroundColor Green
