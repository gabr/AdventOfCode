using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day24
    {

        public (int X, int Y) Get2dCoordinates(string tile)
        {
            int x = 0;
            int y = 0;

            int i = 0;
            while (i < tile.Length)
            {
                if (tile[i] == 'e')
                {
                    x += 1;
                    i += 1;
                    continue;
                }

                if (tile[i] == 'w')
                {
                    x -= 1;
                    i += 1;
                    continue;
                }

                if (tile[i] == 's')
                {
                    y += 1;
                    i += 1;

                    if (tile[i] == 'w')
                        x -= 1;

                    i += 1;
                    continue;
                }

                if (tile[i] == 'n')
                {
                    y -= 1;
                    i += 1;

                    if (tile[i] == 'e')
                        x += 1;

                    i += 1;
                    continue;
                }
            }

            return (x, y);
        }

        public int Solve1(string[] tiles)
        {
            var flippedTiles = new HashSet<string>();

            foreach (var tile in tiles)
            {
                (var x, var y) = Get2dCoordinates(tile);
                var coordinates = $"{x},{y}";

                if (flippedTiles.Contains(coordinates))
                    flippedTiles.Remove(coordinates);
                else
                    flippedTiles.Add(coordinates);
            }

            return flippedTiles.Count;
        }


        public static readonly string[] PUZZLE_INPUT =
        {
            "nenwneswnwnwneswnenwnwsewenwsesw",
            "swsesenwswswseseswseswneswseswsweswnese",
            "neeswsesweneseeenwnenwnewneseewee",
            "wseeeneseneneeeneneenweneewese",
            "wwwwsewewsewnwwswwenewwwnww",
            "seseneeseeswsenwswsewsewseseseneee",
            "enesenesweseeswswswnwwnwne",
            "swwwsenwwnwnwnwenwnenwnwsewsewwne",
            "wnewwseeeswneneneneenweeesw",
            "swwwswnwnwewwseswwesww",
            "nwneswswswnweswweneeeneewwswswswne",
            "senwseswsewseseseesesenwesesenweseesw",
            "senesewneenwnwwesewnwesesesenewne",
            "swwnwnwwwnwnwwewwnenwnwswsenwwnenw",
            "swnwneseewnwnwnenwenewnwnw",
            "swesesweswseswswnwnwsewnwseswseeswsw",
            "nwnwnwsenwnwnwnwnwsenwwenwnwsewnwwneswnw",
            "wwwewnwnwswwwneseeeswnwnww",
            "swswswswseswneswseseneneswneseswswnewsw",
            "swneseswswseseseswswswseswswse",
            "wnewwwswnwnwwwwnwwsenww",
            "senwsenewseneswwnene",
            "nwseeswsesenesenwseseseseswsesenwseseswsw",
            "wwswwswsewnewewwwwwwwwwnenesw",
            "wwnwewenewweswnewswse",
            "swwsenwwnwwwnwwseenwnwwnwnwnwenww",
            "wwnenewswwewwswwwwsewwnwww",
            "swenwnweseeseeeewe",
            "nwewseeeesesese",
            "nwsewesweesenwnwswseesewewswswse",
            "wewsewwswwwnwewwwswnewwww",
            "swswwneswwswswwwswenesene",
            "seseseswswseswseswwseswswnesesw",
            "newnwwwnwwwsewwnw",
            "eswseenwseeeeeeswsenwneeeeee",
            "swswsweswneenwneweneneneeeneesesw",
            "swnwswesweneswswneseswswwswnw",
            "eeeweeeesweneweneeeeeese",
            "esesesesewnwswseswnesesesenweseseseew",
            "nenwsenwnwnwsenenenenenwnewnenwnenesese",
            "neenwneneneenwneneswnenwewneneeswswnenw",
            "swseeswswswswenwswswswnwswswswsesw",
            "nwnwswnwwenwnweneenwswnwsenwswnwsese",
            "wenwweswseswswswswswwnwswswneenww",
            "wswesenenwnwwenwwnwswnwnwnwnw",
            "wnwnwnwsenwnwwnwnwnwwnwnw",
            "ewesweeeeenenenwneswswseeeweee",
            "nwenweeeeweeseeenwsweseeesese",
            "senewneneneneswnenwseenwsenesewnwnenwse",
            "senwseewewseenwseseseseswese",
            "eeeeneswsenweeee",
            "ewnenwnweswnwwswswnwnwnenesesewese",
            "newnwswwnwenwwnwnwswwswnwwwwenw",
            "nwseewwnenwnwnwnwnwnwnwsenwnwnwwenwnw",
            "ewnenenwnenenenenwesewswwnwnenenee",
            "seseswsesesenesewsesesenwseseewnwsese",
            "nwswseseseeseneswnwwseeeesenweswwe",
            "enwnwnesenwnenwnwnenenwwnenwsenwnwsenw",
            "wewnweseeswneenenwseeeeeseee",
            "sesesesesewseswseseseesesese",
            "neeeeeneeswneseenwnenenwneswenwe",
            "sewswswesweswsewseseswnwneeswnwenw",
            "wnenenewswseneeenewseeenw",
            "neswnwnenwwewnwnwnwnwnwseweswnwese",
            "swnwswsewswswnenwnewsewwwwneseswse",
            "wwswsweswwswswswswswswsw",
            "esenewneneseweeenenew",
            "nwswewnwsenwnwenwnwwsewwnesenwnwse",
            "wenwweeseseswewenenweeeesew",
            "sesenwswnwnwnenenwnwnewnesenewnenenenww",
            "sesenesweswwesenwswnw",
            "nwsenwswnenwnwnwsenewneswsenwwsenwnwnwne",
            "nenenwswnwnwnenenwnwnwnwnwsenwneewnesw",
            "nwneneeseeswsenwesewseswwseswwnesene",
            "nwnwnwseneenwnwnwnwneswswnwsweenwnwnww",
            "swnenesewnwseneswnwswseeseswseeenese",
            "newneneneneneeswne",
            "nenwnenwnewnenenenenwneswsesenwnenwnene",
            "swwnwnwewnwnenwnwnwnw",
            "wseenwswsenenewnwswswnwnenwnwswenwsenw",
            "enenewnewnwnwneeneneseswswnwseneew",
            "neneeneeneneswnenewnenwe",
            "senewnwswsenwnenwsenwsewnwwneenwnwsw",
            "nenwneeneeeneneeese",
            "neswswswesewwswwswswsenesewswneswnew",
            "wwsewwnwwwwwwnwnw",
            "neneeeeeeeneswnewneneeseneneew",
            "swswswswwwswswwnesw",
            "seeneneewweenese",
            "eswnwsweswswwnwseswswswneswseeswswne",
            "wwnwswwwnenwnwwnww",
            "nwseeswnwneeseswseseweseneesweseee",
            "eseseseseeseeseseseenwsese",
            "swesenwswseswswsewsesenesese",
            "enesewnwswswnenenenenwneneneswsenwnenw",
            "wnwwsesewwnewswwwwnwnwwwenwe",
            "eswswswseswwnwnwswsweseswenwsw",
            "enwswseswwwswnwswneswswwwseswswswsw",
            "wsenwswswswswsweeneswwwwswwsw",
            "esenwewwwwwwwwwwewwwww",
            "neneneseenwsenwsenwswwwseenewnenesese",
            "nwweswwsenwnwesweweswnwwenwenww",
            "eseswsesesesenesesesesewewsenwnww",
            "newesenwnenenenenwneneneswnesenenenewne",
            "ewnwswnenwnwsenwsenwsenenwnesenwneswnwne",
            "nwwswnwsewnenesesweenwnwnwewnenwnw",
            "swnweswswwswswswswswnwseswswswsw",
            "eewwswnewswewenwnwseneseenwsenwe",
            "nenwswnwenwnenesewnenwseneswseswnwnenene",
            "neneswnwwnwewsewenwsesw",
            "senwseseseseseseswsenwsesesesesesesenesenw",
            "enewsewwnwnwseswnenwswnwwnwwnenwnw",
            "wswwwwswswnweswswwswwwwwwenwe",
            "swswneswseseseseswswsw",
            "wwswsenewwwwwswwswneww",
            "wseeeewneseese",
            "nesewwseseseswseseesw",
            "nwenwnwswnwnwswenwswnwenwnwsenwnenenwnwnw",
            "seswswswseswseneswseswswswneneseenesenwse",
            "wswswwswwswswsweswnwswsweswenwewsw",
            "sewsenwsewseneeswseeswse",
            "eewweewneeneeeseneweeswnene",
            "weeeenwesenweeseseewnewsenwse",
            "nenenewneswneswneneneeneneneeeseneswe",
            "wnwneneseenwewseswswswswneswwswnene",
            "nweeswwwseseeseweswswnwsenwswswse",
            "eneweenenwseweesenww",
            "swnenewnweneseewnenwnesenenwenwnenwnesw",
            "seseswseseswesenesesewswsesw",
            "neeseweneeesewseeeneenwweeseee",
            "newseneswnenwnwnesenenwnwnwenwwnwnwnw",
            "enwnwenwnwwnwnwnwnwnenwnenwswswneese",
            "ewwwwswwwswswwswnewswnew",
            "sesesesenwseseseseseesese",
            "nwnwwsewnwnwnenwwenwswseseenwnwswne",
            "wswneneswswseswseswewwsesesweseneswnwnw",
            "wnwsewwwwnwswwwwwwwenwew",
            "nwnwnwnwnwnenwnwnwsww",
            "wswswswnwswwswnwsenwseneswsweswswseswsw",
            "nesenwneseswseswwnwneseswsesesenwswesese",
            "nwnwnenewsenwneseenwnenwnwnwneseneswnwswne",
            "nenesesenenenwnwswneeewnenenw",
            "swnwswseswswwnwwwwswswnenewsewswswese",
            "swenenwnwnwnenenwswnenwnenesweenenwnw",
            "eeswenewenwneswneneeneenenenee",
            "swswsweswswneswswswnenwswsweswnwsenwne",
            "eeeesesewesenwesw",
            "wwswswseneswsesenwseseneseeswseenwnwsw",
            "swwwwewswwesw",
            "wswwswwnewwseswswswswewswnwwnesw",
            "eswsenwseswneewenweeewwwneene",
            "wwewweswswwwswwewswwnwwwnw",
            "swwneeenwnenwnwwswnenwnwnwweenwne",
            "eneseneneswneneeneseneenweweswwnw",
            "nwesesesesweeseneseesesesesesenwsenw",
            "wwwwwwwwneswwseswwnesewwswesw",
            "wnwesenwnwsenwwwnwwnwnwwwnewswwnw",
            "eneewneeeneneneneneeenewese",
            "sesesenesewsesesesesesenwsesesesenw",
            "seswsenenwwsewseneneseeseswswswsesesese",
            "swnenwswnwnewnwswswwnweewsesesewwne",
            "ewneswswneswnwnwwsewneneseswseswsesee",
            "wswswwswwnewwswwswswwewswsewe",
            "swwnwnenenesewnenwwnwneneeweeswsene",
            "eseswseseeseeeeneeswneneneweswee",
            "eseseseenenesesesesewneseseseseseww",
            "enesenenenwneneneneenenene",
            "wswwswwwnewnwwsenewnwwnewseew",
            "nenenenwneneeenwnwwswswnenenwenwnesw",
            "nenenwnewswneneneneseneenenenenenesene",
            "nenenwnwnwnwsenwnwnwnw",
            "swwnweenwneneneeswnenenenenewswsenenenw",
            "eswnenwswswnwwewneseesweseneenww",
            "nenenenenwwnenwneneenenenw",
            "sesewswneswswswseswneswswsenwswseswswse",
            "esesweeeeneneesweeeenwwnwneee",
            "wwwwnwwnwwsenenwwwwwnwwswseew",
            "nweseneswnenenenenenenwnenwnenwne",
            "wnwnwsenenewnwsewsenenenwewsenwwee",
            "wnwseswsesesweesesenwseswswse",
            "nwnenwwnwwneneneeswseneneeswnwnesene",
            "neswsesesesewsesesenwswseesesesesewnesese",
            "nwwwswwwewwww",
            "swsweseseseseneswnwnesewseseswseseswsw",
            "esesenwseseswwseseesenwnenesesesewsew",
            "eseswswnwswnwsesewesesweswsw",
            "wwwnwwwenwsenwnwnwwnw",
            "wnwnwnwnwnwnwwnwnwnwnwnenweesenwwswnwsw",
            "swswsesweenwnwwnene",
            "swswswwneneneswwsesenwwswnwnwwwsee",
            "nenwsenwenwenwsenenwnwnwnwneswnenwnww",
            "swewwnenweswnwswwnwnwwwnwnwwwww",
            "newneneenenenenenenenenee",
            "swewnwswswneeswseseswnesenwenwwswsesw",
            "wnewwswenwwwwswnwwwnewnwnwsww",
            "nesenwswsweneseswneesesewswnenwsesesese",
            "sewswswswwwnewswswwnwwnewwwswwsee",
            "seeeewswneseewsesenenweseesesese",
            "nenwneswnenenwnwnwwnwenwenenenwnwnwsw",
            "enenwsenwwnwneswseseseenewseswsenww",
            "newswswwswwswswwswnwwwswswneesesw",
            "nwswswnwnwswseswswwneseenwwswe",
            "nwwwswseneneneneneenwneneneneneenenene",
            "senwsenwwwwwsewwwwneswnenwwww",
            "swswwswsenwswseesesewnenenenewswsweswnw",
            "swweneenwwsenwneneeneseneeene",
            "nwnwnenwswenwnwwnwewnwnwsenwnenwnewnwe",
            "eenenwneswnenenese",
            "senwnwneseseeewswsesewsewenesesese",
            "neswwwewwnenwwswwseswnw",
            "wseeeneswseseeseseseseneneseseswsese",
            "senenewesenwesenwswwwswnwsesenwenw",
            "nweneneeeseeeeeseenwweeeswne",
            "wswswseseswswswwnwnenwsweswswswswwsw",
            "nenwswnwenwneswseesenenenewneee",
            "nwnwnwnwnwnwenwswnwnwnwwnw",
            "seseseseseneswseseseswswsw",
            "eswswnewwswsenweswwwnwswswesenww",
            "nenenenwwnwneenenesesenenewne",
            "neeeswneswnweeeeeneneeswneenenew",
            "weswswenwseswswswswsenwswsw",
            "nwnwnwnwnwwenewwneewseseswnww",
            "nwwsenwwswswneeneswswswsesweswswnesenee",
            "wswwneswswswweseswnewnewnenesesesenenw",
            "sweseseswswswsewsw",
            "eeenewseneeweseenenewneseeneenee",
            "seseenwnwseseseeseseeeneweseseseew",
            "wwewnwnwwwsweew",
            "sewnwnenwnwesenwwnenwnwwnwneswwwsew",
            "wswwwswwsenwnwwwneswewswenwswswwsw",
            "nwnenenenenewenenenenenwnene",
            "sewnesewsewsewnenwwwwnewnwnwww",
            "nwsewneeswwsewneswnwweenwnenw",
            "eseeseseneseseweesesenwsesese",
            "eneenenewenwswnwseswsenweeeeee",
            "sesenwweesesesesesesenesewsesesesese",
            "eneenwneeeewseenewewnweswseswe",
            "swwswswswswswweswnweswswswswsesweswswnw",
            "eewseeeenweeeenweswseseesee",
            "nwnwneeswnwnwwnwwswswneswswnwnwenwswene",
            "eenwnwswesenwsweswsesenweseenwee",
            "wwnwwwewwnwwnenwwwsw",
            "swnwnwneswenwenenenewenwswseswewse",
            "nenwnwnenwnwnwnwnwswnwnwnwnw",
            "nweswneeeswnweswswenwsweswneeenenwne",
            "nwnenwneesewnewnwesenwnwnenwnewnwnwnww",
            "ewwwwswswwenwwswswswwsesw",
            "nwesewnenwesewseswnenweswseewseseee",
            "nwswwsweswswseswwswswswnwswnwswswwe",
            "wenwseewseeseneeseswswseseenwsene",
            "seswswsesesesesesesenw",
            "enenewenenenwnenwnenenwnenwswseswne",
            "swwwsesewwwnwsewwsewnenwnwwwwww",
            "wsenwwnenwnwsenwseseseseswseswneenesese",
            "sweeeenwsenwnesenenwswswewwnwesww",
            "swseswswswswsenwswswsw",
            "nwswnwswnwnenwnwnwenwnwnwnenwnewnwnwnwsew",
            "weeeeseweseweneenwsweeseenew",
            "nwswswseswswswseswswswsw",
            "eswsenwswsesenwswneneeenwneseswwswse",
            "seseseseeseseseeenewsesese",
            "swnenesweweenwneeeneneseeeeee",
            "wwwwnwwwsewwnesewwnewse",
            "neeenenwnenwswswnwseneeeeesweee",
            "neenenenenenenwenewnenwswneswenwnene",
            "wseeenesesenwneesenweeseswswswswse",
            "esenwweswwewnenwewseenwsenwsee",
            "newneeseeseeswewseeesesenweeesee",
            "esewnwsenenewswnwwwsenwseswnwewnww",
            "neneneeneenwewweneswseneneneneese",
            "neewswnwweseswneesweswnwenwenenw",
            "wwnwwnwwwwwwwsewewww",
            "seseseneseswsesewseswseseswnwsee",
            "esweeswseweseseeeenwswenwneene",
            "sesenwwseseneseswswnwswsewseseseseseene",
            "eeesenwsweeenwneeswswneesenewne",
            "nenwnwnenenwnenenwwewewnwesenwswnw",
            "nwnwnwwnwseenwenwnwnenwnwnwwnwnwwne",
            "wneseeeenenewnwnwwneneesweswnene",
            "wwnwwwnwsenwwewwnwsenwnwwwnew",
            "nwnwnwnwnwwnwnwwswsesenewenwnwnwnwww",
            "eswwsewnwwsewwnwwwwewwwenwsw",
            "nesweswnewswnwswswewwnwsenwsenwwnwse",
            "nwnwwwnwwweweswsenwwsewwneeww",
            "neeenwswswnwswsweneneenwneeeneeneee",
            "nwnwnesenwsewnwnenenwsewnwnee",
            "eeenwesenwsesenweesweseseeswesee",
            "eewwwwswwswswwswnewwnwnwesww",
            "sewnenesenwnwswswnenwseneswwnenwnenwesw",
            "ewswnwnwnwenwnwnwnwnwwswnwwenwenw",
            "nwnwnwnwsenwnwnwnwnwnwnwnwwnw",
            "newwwswswswswseswswswswswesw",
            "swswswwwswwesenwswwneswswswwswsw",
            "neeeenenweeeneneesw",
            "seesesenwenwsewseenwsewswnweneew",
            "swnesenesenesewsweneeswnwsesenwwese",
            "eswnwswwswsenesenew",
            "swneswwenwnenwsenenewwswesenenenenw",
            "wwwwwwwwnwswneeseswneesesw",
            "sesenwnwwnwwnesenwnwnwwnwswenwnwwene",
            "swswswwsesweseswswwswnwswswsesesenee",
            "nwnwswwnenenwnwwnwwnweswnwsenwnwnwnwnw",
        };

    }
}

