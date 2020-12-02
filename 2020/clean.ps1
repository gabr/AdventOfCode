dotnet clean

Remove-Item publish -Recurse -Force -ErrorAction SilentlyContinue -Verbose

function Remove-ProjectDirectory ($filter)
{
    Get-ChildItem -Recurse -Depth 1 -Filter $filter -Name |
        Remove-Item -Recurse -Force -ErrorAction SilentlyContinue -Verbose
}

Remove-ProjectDirectory obj
Remove-ProjectDirectory bin
Remove-ProjectDirectory TestResults

