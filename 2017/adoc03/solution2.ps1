# 289326 -> 295229 {1, 4}
# 312051 -> 312453 {0, 4}

$all = @(
    ([PSCustomObject]@{Value = 1; Point = @(0, 0)})
)

$point = @(0, 0)
$steps_in_direction = 0;
$steps_to_change_direction = 1;
$steps_to_change_direction_change = 2;
$direction = "right"

$all
for ($s = 1; $s -lt 100; $s++) {
    switch ($direction) {
        "up"    { $point[1] += 1 }
        "down"  { $point[1] -= 1 }
        "left"  { $point[0] -= 1 }
        "right" { $point[0] += 1 }
    }

    $v = 0
    foreach ($a in $all) {
        if ($a.Point[0] -eq ($point[0]-1) -and $a.Point[1] -eq ($point[1]))   { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0]+1) -and $a.Point[1] -eq ($point[1]))   { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0])   -and $a.Point[1] -eq ($point[1]-1)) { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0])   -and $a.Point[1] -eq ($point[1]+1)) { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0]-1) -and $a.Point[1] -eq ($point[1]-1)) { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0]+1) -and $a.Point[1] -eq ($point[1]-1)) { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0]-1) -and $a.Point[1] -eq ($point[1]+1)) { $v += $a.Value }
        if ($a.Point[0] -eq ($point[0]+1) -and $a.Point[1] -eq ($point[1]+1)) { $v += $a.Value }
    }
    $all += [PSCustomObject]@{
        Value = $v
        Point = @($point[0], $point[1])
    }
    $all[$s]

    $steps_in_direction += 1

    if ($steps_in_direction -eq $steps_to_change_direction) {
        $steps_in_direction = 0
        switch ($direction) {
            "up"    { $direction = "left" }
            "down"  { $direction = "right" }
            "left"  { $direction = "down" }
            "right" { $direction = "up" }
        }

        $steps_to_change_direction_change -= 1
        if ($steps_to_change_direction_change -eq 0) {
            $steps_to_change_direction_change = 2
            $steps_to_change_direction += 1
        }
    }

}

