"removing obj"
dir -recurse -directory -filter obj -name | % { rm $_ -Recurse -Force }

"removing bin"
dir -recurse -directory -filter bin -name | % { rm $_ -Recurse -Force }

"removing exe"
dir -recurse -file -filter *.exe -name | % { rm $_ -Recurse -Force }

