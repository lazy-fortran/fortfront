param(
    [Parameter(Mandatory = $true)]
    [int] $Seconds,

    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $CommandParts
)

if ($Seconds -le 0) {
    Write-Error "Timeout must be positive seconds"
    exit 64
}

if (-not $CommandParts -or $CommandParts.Count -eq 0) {
    Write-Error "No command specified"
    exit 64
}

$exePath = $CommandParts[0]
$argumentList = @()
if ($CommandParts.Length -gt 1) {
    $argumentList = $CommandParts[1..($CommandParts.Length - 1)]
}

$process = Start-Process -FilePath $exePath -ArgumentList $argumentList -NoNewWindow -PassThru

if (-not $process.WaitForExit($Seconds * 1000)) {
    try {
        $process.Kill()
    } catch {
        # ignore kill race
    }
    exit 124
}

exit $process.ExitCode
