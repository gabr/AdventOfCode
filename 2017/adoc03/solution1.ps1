function solve1($v) {
    $point = @(0, 0)
    $steps_in_direction = 0;
    $steps_to_change_direction = 1;
    $steps_to_change_direction_change = 2;
    $direction = "right"

    for ($s = 1; $s -lt $v; $s++) {
        switch ($direction) {
            "up"    { $point[1] += 1 }
            "down"  { $point[1] -= 1 }
            "left"  { $point[0] -= 1 }
            "right" { $point[0] += 1 }
        }

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

    return $point
}

#1..1024 | Foreach {
#        $p = solve1 $_
#        "$_ -> [$($p[0]), $($p[1])] "
#    }

solve1 312051

