using System;

namespace Solutions
{
    public class Day03
    {
        private bool IsTreeAt(int x, int y, string[] map)
        {
            var row = map[y];
            x %= row.Length;
            return row[x] == '#';
        }

        private long CountTreesOnSlope(string[] map, int xDelta, int yDelta)
        {
            int x = 0;
            int y = 0;
            long treesCount = 0;

            while (y != map.Length -1)
            {
                x += xDelta;
                y += yDelta;

                if (IsTreeAt(x, y, map))
                    treesCount += 1;
            }

            return treesCount;
        }

        public long Solve1(string[] map) => CountTreesOnSlope(map, 3, 1);

        public long Solve2(string[] map)
        {
            return
                CountTreesOnSlope(map, 1, 1) *
                CountTreesOnSlope(map, 3, 1) *
                CountTreesOnSlope(map, 5, 1) *
                CountTreesOnSlope(map, 7, 1) *
                CountTreesOnSlope(map, 1, 2);
        }

        public static readonly string[] PUZZLE_INPUT = new string[]
        {
            ".#....#..##.#..####....#.......",
            "......#..#....#....###......#.#",
            "#..#.....#..............##.#.#.",
            "#.#...#....#...#......##..#..#.",
            "...#..#.##..#..#........###.#.#",
            "...#.#..........#.........##...",
            "...#.#....#.#....#..#......#...",
            "..##.#.....#.......#.###..#..##",
            "..#.......#.......#....##......",
            "....##........##.##...#.###.##.",
            "#.......#.......##..#......#...",
            "..##.............##.#......#...",
            "...#.####....#.....#...##......",
            ".............##.#......#.......",
            "..#...#....#......#....#.......",
            "..#....#..#............#.......",
            "##...#..#........##..#......#..",
            "##........##........#.#.......#",
            "#.......#........#.#..#....###.",
            ".....#..#.#..........##...#....",
            "..##...#......#.#...#..#...#...",
            "##.##...#......#....#....#...#.",
            "#.......#..#.#..#....#..###.#.#",
            "#.............#.#....#..#.#....",
            "...#.......###.#.##.##.#...#..#",
            ".##.......##..##...#..###......",
            ".......#.#.#.#..####..#..#..#..",
            "...##......#.#.##.###....#.###.",
            "###......###......#.#####..#...",
            "..#........##..#..##.##..#...#.",
            ".....##..#...#..#.##.....#.#...",
            "#......#.##....#..##.#....#.#..",
            "##.#.##..#................#....",
            "......#.#....#......##.....#...",
            "..#...##..#..#...#..#.#..#.....",
            "........#.#.#.##...#.#.....#.#.",
            "#.#......#.....##..#...#.......",
            "..#.#......#...........###.....",
            "......##....#....##..#..#.#.#.#",
            "##....#.###...#......#..#...#..",
            "#.#.##....###...####.......#..#",
            "##...........#.....#........#.#",
            ".##.#..#.....#......#.......#..",
            "##..##..###....#.........##....",
            "..#..#..#.##...#.#...#........#",
            "#.##.###...#.......#...........",
            ".........#.................#...",
            "#.##...#.....#..##........#....",
            "....#.#...##...#...........#...",
            ".#.....#.#..#...##..##.....#...",
            ".#.....####....#..##..#........",
            "...#..#......##.#..##.#.#.#..#.",
            ".##.#.....#.....#...#.......##.",
            "......#..#..#......#...####....",
            ".......#......##..#..##.....#..",
            "......#.#..#...#..#.#..........",
            "....##.........#...............",
            ".#....#..##.....#....##.##.....",
            "#.#.....#...#....####....#.#...",
            "#.....#....#.#...#.............",
            "...#..#.....#....##..#..#......",
            "...#.#............#...........#",
            "###.#..#.#......#.....##.....#.",
            "####....#....###.....#..#.#####",
            ".###..#...#.#..#......##.#.#.#.",
            ".....#.##.#....#..##....#..#..#",
            "...#....#...##.....#......#.#..",
            "....#...#....#...#....#.....#.#",
            ".#.....#.....#.#..#......#..#..",
            "..#..##....##.##....#.....##...",
            "#..##...#.##...#..#.#.#.....#..",
            "...#..##.#..#....#.#....######.",
            "..........#..#.....#....#...##.",
            "#.#####.#.###..#.....#.........",
            "#....#......#..#.#.##.##..###..",
            "..#...###.#.#....##.##...##....",
            ".......#....#..#...##......#...",
            "...#.#...#..#.....#..##.#......",
            "###..##...........#............",
            "..#....#.##....#.#..##...#.....",
            "##....#...#....#.....#.#..##...",
            "..............#.##.#..#..##.###",
            "......#..#..#..#.#....###...##.",
            ".#...#..#.#.#....#..........#..",
            "..#.#.....#..#...........#.##..",
            "...#.#......#......#..#..#.#...",
            "...#....#.#.#.....#...#.##..#..",
            ".#.#..#...#........##.......#..",
            "##..........#..#...#....###.#..",
            "#.....###......#..#.#.#....#.#.",
            "..###.......#.#...............#",
            "#....#.....#.#......#..##.##...",
            "#.##.#.#....#..#.#...#.#...#..#",
            "#....#..#...........#.......#..",
            "...#.####.....#.........###.##.",
            "......#..#.....#..........#..#.",
            "#...#.#..####...#...#.#.##...##",
            ".##.........#......#.#.#.......",
            ".......##...##.##....###...##..",
            "...#..#....#..#.#.#.....#.#....",
            "#....#.#.#.......##..###..##...",
            "......#............#.#...#..#..",
            "#.#.....#......#...#.......###.",
            "...#.#................#...#....",
            ".....#......#.#..#...##.#.#...#",
            "#....#.#..#..#..##..#.##..#....",
            "#.................#..#....#....",
            "..#....#.......####....###.....",
            ".#..#.#.#...###..#...#..###....",
            "..#..#.#......#.###..........#.",
            ".....#......#.......##....##.#.",
            ".#....#........#.#.##.#........",
            "#.#..##..#..#.#...####....##...",
            "...#....#.#..#...#..........#..",
            ".#.....#.##....#...##..........",
            "....##....#.....#.....#...#.###",
            ".#...##.#.#..##..#...#.#..#..#.",
            "..#.......#.##.....#.#........#",
            "...#...#.....##..#.#.#....#....",
            "...#.....#.......##.........#.#",
            ".##.....#..#.#...#.#...#.#...#.",
            "...........#...#.###..#...#..#.",
            "#.##........#..###.##...####...",
            ".#.....#.#...##...#..#..#...##.",
            "..#....#..#...#.....#.....##...",
            "..###..#.#.....##........#.##..",
            ".#.#..##........#.##....#..#.##",
            ".####.#..##..#.#..#....##....#.",
            ".##....##...#.#........#.......",
            "....#..#..#...#..#..#..#.#.....",
            "...#......................#....",
            "#.....#.#....#..#..#.#..#....#.",
            "##.....#.....##..........###...",
            ".#..#..............#...##.#.#.#",
            "...#...#.#.............#.#..#.#",
            "..#.....#.......#......#.#.....",
            ".###.#..#..#..#.#..#.....#.....",
            ".....##..##...##.......#....###",
            ".#........###...##..#....##....",
            "#....#.#......##..#....#.##..#.",
            "#....#.#...#........##...###...",
            ".#.....#...#.###....#.##.#.####",
            "###......#....#...#....##..#..#",
            "##....#..###......#...#.#.#....",
            "..........#......##.#..#.......",
            ".#..#......###.........##...#..",
            "....#......#....#.........#.#.#",
            "##.#.#...#.#...#...#..#......#.",
            "....#.##.........#..#.....##.#.",
            "........#...#..#.#.#.#.....##..",
            "..#......#.#.#..#.....##.......",
            "..............#....#....##.#..#",
            "....#.#.....#....#.#.###.#....#",
            "..#..........#..#......#.##..#.",
            "...#...#.#.............#..#....",
            "#.......#..#..##.........##..#.",
            "..##..#............#.....#.....",
            "....#.#..##...#.#..#.........#.",
            "........#.......#.##....#....#.",
            "...#.....#.#.....#.#....#......",
            "..#......##.#.............#.#.#",
            "#.#.............#.#.....#......",
            ".##....##.#.....#....#...##....",
            ".#.#....##....#.....##.........",
            "...#.....#.....#.....#..#.###..",
            ".......#....#...##.#...#...#..#",
            "..#.#.......#...###.#...#....#.",
            ".....###..##....###.#.##.......",
            "....#..................##.#.##.",
            ".#.......###.##...#.#.....#....",
            "....#....##...##.....#.#...#..#",
            "#..#.....#......##...#....#....",
            "#..##.........#.....#...#......",
            "...#..##.......##......#..#####",
            ".#..###.###.#.##........#......",
            ".#...#....#....#.#....#...##...",
            "##........#....#.........##..#.",
            ".#.....##............#.#.......",
            "....#....#...........###.....##",
            ".#......#.#.#..#....#.#.....##.",
            "......#.##.#..##....#.#.#..#...",
            "#....#......#...#..####........",
            "......#..#..##..#.......#.#..#.",
            "##....##.###.#...#.##.#..#.###.",
            ".#.........#...##...#.#......#.",
            "..#.#...........####.#....##.##",
            ".....#.#..##.#...###...#..#.#..",
            "...#..#..##.#...#.....#.##...##",
            "..##......##..........#..###...",
            ".#......##.....#.##....#.#.##.#",
            "...#.......##..##.....#....#...",
            ".##...#...#....#..#............",
            "#..#....#...........#..........",
            "......#...#.#.......#...#.##..#",
            "..#.###..#.....#.....#..#.....#",
            "....#....#..........##....#..#.",
            ".......##.#.#.#......#....#...#",
            "####..#.###........#..#......#.",
            "#........##.#.#.#.............#",
            ".#......#......#..#.##.....#...",
            ".....##.##......##.#.....#.#.#.",
            ".......##.#.....##.......#.#.#.",
            ".#.#..#.#..#.##...#.#....#.#..#",
            ".#..##....#..#...##.......#..#.",
            ".#.#..#.......#................",
            "#........#.#.#......#.#.#.#....",
            "##......#...#..##.#...##.##....",
            "##.#..#...........##...#..###..",
            "......#.####...#........#.#.#..",
            "...#........##..###.#.#...#...#",
            ".#.....##..#......##......###..",
            "..#.#...#......#..#..##.#.....#",
            "#....#..#.#..........#...#.....",
            ".#......#.##..###..#.#....#..##",
            ".......#.......#..#..#......#..",
            "..##.....##.#..#..#.....##.....",
            "........#.##...#.#.#..#..#..#..",
            "...#.######.........#.....#..##",
            ".#.#............#....#.........",
            "#...#....###.#......#.#........",
            "#.........#....#...##..........",
            "....#...........#.###.#...###..",
            ".........#........#.#.#..#...#.",
            ".#.......#.#.....#.#.....#.##..",
            ".....#.......#.....#.#.#.......",
            "#.##..#..##.......#...#......#.",
            ".###.....##...##.#...##.##.#.#.",
            "...#......##..##............#.#",
            "...#......................#..##",
            "#..#..#................#...#...",
            "#..#....#.#.#...##.......#..#..",
            "....#.#..###.##...#..#.###..#..",
            "..#...#....####.#............#.",
            "......#....#.#...#.#.#.........",
            "#...#........#.....##..###.#..#",
            "#....#...#...##...#..#....##...",
            "#..#...#..#.......#.#..##.#..#.",
            "#.#..........#...........##....",
            ".#...###...#......#.......#.#.#",
            ".........#.........#...#...##..",
            "##.#..###......##..#.....#..#..",
            "....##...............#.....#...",
            ".....#.....###.#.....#.#.......",
            "....#..#......###..#..##..#....",
            "......................#.....#..",
            "..#..#...##....##....#........#",
            "..#....#...#...#.......#.....#.",
            "...##.#.#.##......#.#.#.#.####.",
            ".###...#..#......#.#..#........",
            "#..#...##.#..#..##..##....#...#",
            "...#...#..#..#..#........#..##.",
            ".##....#.##.#....#...#.#.#....#",
            "#..#....#..#....#.......##..#.#",
            "...#.#....####...........#...#.",
            "#...#####...#.#..#......#...#.#",
            ".##....#.#.#..#......#..##.....",
            "..........#..#.#.#.....##......",
            ".....#....#..................#.",
            ".........#...#...#....#..###...",
            ".#.#.#....#....................",
            "......##............##.###..#..",
            "#.#...#........####.##..#.#.##.",
            "#........#.#.#.#..#.##.....#...",
            "......####..#.##.......#....#..",
            ".........#...#...#.....#.......",
            "..##.....#...#...#.....##.....#",
            "....#...##....#.....#..#..##.##",
            "..#.........##...##..###..#....",
            "#....#.#.........##.###.#...##.",
            ".##...#....#..#..#.#....##.....",
            "##..#..#..#...........#.##....#",
            "....#..........#...#..#.....#..",
            "........###..#..#.#.#.....##...",
            "#...#...#..###............###..",
            "..#.....#.#.#..#..#.#..#......#",
            "..#...##..#....#...#......#....",
            "#....#........##.....#..##....#",
            "#.....#.#.#..#.......##.#.#.##.",
            "..##...#...#.....#..........#..",
            "##.....#....#......#..........#",
            "......#..#..........#.#..####..",
            "......#...#............##...##.",
            "..#.......##.......#...###.###.",
            ".#..#.#.#...#..##.#......#.#...",
            ".##.....##.#.#...#.##.........#",
            "#.#.######...........#.#####.#.",
            "........#.##...##....##.#.##.#.",
            "....#......#.....#.....###...##",
            "#..............#.#....#.#....#.",
            "....#..###.#.........##.#.#....",
            "..#.#.#..##....####..........#.",
            "...#..#.......#................",
            "...#....#..............#....#..",
            ".....#...#...#....#.#.#..#...#.",
            "......##.............###.##..##",
            ".#...#.#..#......#..#.##.......",
            "##.....#.....#.##...#....#.....",
            "..#..#.#.#.#.#..........#..###.",
            "##..........#........#....#.#..",
            ".....#...#........#.#..###....#",
            ".###.#........#.##......#.#...#",
            "#...##....#....#....##.#.#.....",
            ".....#.#............#..........",
            "..#.##....................#....",
            ".....#..#..#.#..#.##.......#...",
            ".....###......#......##......##",
            "#.....#.#.......##.......#...#.",
            ".#.#...#......#..###...#.....#.",
            "#.#..#...#..##.....#...#.#..#..",
            ".....#.#..........#..#.........",
            ".###..##..##.....#...#...#..##.",
            "#...#.#....#.......##..#.......",
            "###...#.#.#..#.......#......#..",
            "....##........#..........##....",
            "............#....#...........#.",
            "#..#.#....##..#.#..#......##...",
            ".###....##...#....##..........#",
            ".###........#........###.....#.",
            "...#...#.#......#...#....#.....",
            ".###.......#.........#.........",
            "....##.#......#...###......##.#",
            ".###...#..##.....##.......#....",
            ".#.#...#..#.##....#........#...",
        };
    }
}

