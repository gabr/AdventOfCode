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

        public string CoordinatesToString(int x, int y) => $"{x},{y}";

        public (int x, int y) StringToCoordinates(string xy)
        {
            var s = xy.Split(',');
            return (
                int.Parse(s[0]),
                int.Parse(s[1])
            );
        }

        public HashSet<string> FlipTiles(string[] tilesToFlip)
        {
            var flippedTiles = new HashSet<string>();

            foreach (var tile in tilesToFlip)
            {
                (var x, var y) = Get2dCoordinates(tile);
                var coordinates = $"{x},{y}";

                if (flippedTiles.Contains(coordinates))
                    flippedTiles.Remove(coordinates);
                else
                    flippedTiles.Add(coordinates);
            }

            return flippedTiles;
        }

        public int Solve1(string[] tiles)
        {
            var flippedTiles = FlipTiles(tiles);
            return flippedTiles.Count;
        }

        public int Solve2(string[] tilesDirections)
        {
            int size = 500*2;
            var tilesGrid = new bool[size * size];

            int GetTileIndex(int x, int y) => x+(y*size);
            (int x, int y) GetCoordinatesFromIndex(int index) => (index % size, index / size);

            int GetTileIndexFromDirections(string directions)
            {
                int x = size/2;
                int y = size/2;

                int i = 0;
                while (i < directions.Length)
                {
                    if (directions[i] == 'e')
                    {
                        x += 1;
                        i += 1;
                        continue;
                    }

                    if (directions[i] == 'w')
                    {
                        x -= 1;
                        i += 1;
                        continue;
                    }

                    bool isEven = y % 2 == 0;
                    if (directions[i] == 's')
                    {
                        y -= 1;
                        i += 1;
                    }
                    else if (directions[i] == 'n')
                    {
                        y += 1;
                        i += 1;
                    }

                    if (isEven)
                    {
                        if (directions[i] == 'w')
                            x -= 1;
                    }
                    else
                    {
                        if (directions[i] == 'e')
                            x += 1;
                    }
                    i += 1;
                }

                return GetTileIndex(x, y);
            }

            foreach (var tileDirections in tilesDirections)
            {
                int index = GetTileIndexFromDirections(tileDirections);
                tilesGrid[index] = !tilesGrid[index];
            }

            int flipedTilesCount = 0;
            for (int i = 0; i < tilesGrid.Length; i++)
                if (tilesGrid[i])
                    flipedTilesCount += 1;

            Console.WriteLine($"FlipedTiles: {flipedTilesCount}");

            int[] GetNeighbours(int index)
            {
                (int x, int y) = GetCoordinatesFromIndex(index);

                if (y % 2 == 0)
                {
                    return new int[]
                    {
                        GetTileIndex(x+1, y  ),
                        GetTileIndex(x-1, y  ),
                        GetTileIndex(x-1, y+1),
                        GetTileIndex(x-1, y-1),
                        GetTileIndex(x  , y+1),
                        GetTileIndex(x  , y-1),
                    };
                }
                else
                {
                    return new int[]
                    {
                        GetTileIndex(x+1, y  ),
                        GetTileIndex(x-1, y  ),
                        GetTileIndex(x+1, y+1),
                        GetTileIndex(x+1, y-1),
                        GetTileIndex(x  , y+1),
                        GetTileIndex(x  , y-1),
                    };
                }
            }

            bool AmIBlackInNextRound(int index)
            {
                var amIBlack = tilesGrid[index];

                var neighboursCount = 0;
                var neighbours = GetNeighbours(index);
                foreach (var neighbour in neighbours)
                    if (tilesGrid[neighbour])
                        neighboursCount += 1;

                // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
                if (amIBlack)
                {
                    return !(neighboursCount == 0 || neighboursCount > 2);
                }
                // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
                else
                {
                    return neighboursCount == 2;
                }
            }

            for (int i = 0; i < 100; i++)
            {
                var nextRound = new bool[tilesGrid.Length];
                for (int ti = 0; ti < tilesGrid.Length; ti++)
                {
                    if (tilesGrid[ti] == false)
                        continue;

                    if (AmIBlackInNextRound(ti))
                        nextRound[ti] = true;

                    var neighbours = GetNeighbours(ti);
                    foreach (var n in neighbours)
                        if (AmIBlackInNextRound(n))
                            nextRound[n] = true;
                }

                tilesGrid = nextRound;
            }

            flipedTilesCount = 0;
            for (int i = 0; i < tilesGrid.Length; i++)
                if (tilesGrid[i])
                    flipedTilesCount += 1;
            Console.WriteLine($"Koniec: {flipedTilesCount}");

            return flipedTilesCount;
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

