using System;
using System.Linq;
using System.Collections.Generic;

namespace Solutions
{
    public class Day11
    {
        public char[][] GetMapFromInput(string[] input)
        {
            return input
                .Select(s => s.ToCharArray())
                .ToArray();
        }

        public int CountNeighbours(char neighbour, int x, int y, char[][] map)
        {
            int sum = 0;

            if (x + 1 < map.Length)
            {
                if (map[x + 1][y] == neighbour) sum += 1;

                if (y + 1 < map[x].Length)
                    if (map[x + 1][y + 1] == neighbour) sum += 1;

                if (y - 1 >= 0)
                    if (map[x + 1][y - 1] == neighbour) sum += 1;
            }

            if (x - 1 >= 0)
            {
                if (map[x - 1][y] == neighbour) sum += 1;

                if (y + 1 < map[x].Length)
                    if (map[x - 1][y + 1] == neighbour) sum += 1;

                if (y - 1 >= 0)
                    if (map[x - 1][y - 1] == neighbour) sum += 1;
            }

            if (y + 1 < map[x].Length)
                if (map[x][y + 1] == neighbour) sum += 1;

            if (y - 1 >= 0)
                if (map[x][y - 1] == neighbour) sum += 1;

            return sum;
        }

        public bool IsSeatedInDirection(int x, int y, int dx, int dy, char[][] map)
        {
            int tx = x + dx;
            int ty = y + dy;

            if (tx < 0 || tx >= map.Length)     return false;
            if (ty < 0 || ty >= map[tx].Length) return false;

            if (map[tx][ty] == 'L') return false;

            if (map[tx][ty] == '.')
                return IsSeatedInDirection(tx, ty, dx, dy, map);

            if (map[tx][ty] == '#')
                return true;

            throw new NotImplementedException($"Unknown symbol: '{map[tx][ty]}'");
        }

        public int CountVisibleOccupiedSeats(int x, int y, char[][] map)
        {
            return
                (IsSeatedInDirection(x, y, +1,  0, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y, -1,  0, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y, +1, +1, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y, +1, -1, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y, -1, +1, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y, -1, -1, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y,  0, +1, map) ? 1 : 0) +
                (IsSeatedInDirection(x, y,  0, -1, map) ? 1 : 0);
        }

        public int CountCharOnMap(char c, char[][] map)
        {
            int sum = 0;

            for (int i = 0; i < map.Length; i++)
            for (int j = 0; j < map[i].Length; j++)
                if (map[i][j] == c)
                    sum += 1;

            return sum;
        }

        public void AdvanceMap(ref char[][] map, ref char[][] buffer)
        {
            for (int i = 0; i < map.Length; i++)
            for (int j = 0; j < map[i].Length; j++)
            {
                int neighboursCount = CountNeighbours('#', i, j, map);

                if (map[i][j] == 'L' && neighboursCount == 0)
                {
                    buffer[i][j] = '#';
                    continue;
                }

                if (map[i][j] == '#' && neighboursCount >= 4)
                {
                    buffer[i][j] = 'L';
                    continue;
                }

                buffer[i][j] = map[i][j];
            }

            var tmp = map;
            map = buffer;
            buffer = tmp;
        }


        public void AdvanceMap2(ref char[][] map, ref char[][] buffer)
        {
            for (int i = 0; i < map.Length; i++)
            for (int j = 0; j < map[i].Length; j++)
            {
                int neighboursCount = CountVisibleOccupiedSeats(i, j, map);

                if (map[i][j] == 'L' && neighboursCount == 0)
                {
                    buffer[i][j] = '#';
                    continue;
                }

                if (map[i][j] == '#' && neighboursCount >= 5)
                {
                    buffer[i][j] = 'L';
                    continue;
                }

                buffer[i][j] = map[i][j];
            }

            var tmp = map;
            map = buffer;
            buffer = tmp;
        }
        public void PrintMap(char[][] map)
        {
            for (int i = 0; i < map.Length; i++)
            {
                for (int j = 0; j < map[i].Length; j++)
                    Console.Write(map[i][j]);
                Console.WriteLine("");
            }
        }

        public int Solve1(string[] input)
        {
            var map = GetMapFromInput(input);

            var bufferMap = new char[map.Length][];
            for (int i = 0; i < bufferMap.Length; i++)
                bufferMap[i] = new char[map[i].Length];

            int seatedPersonsCount = 0;

            while (true)
            {
                AdvanceMap(ref map, ref bufferMap);
                var count = CountCharOnMap('#', map);

                //Console.WriteLine(count);
                //PrintMap(map);
                //Console.WriteLine("");
                if (seatedPersonsCount == count)
                    break;

                seatedPersonsCount = count;
            }

            return seatedPersonsCount;
        }

        public int Solve2(string[] input)
        {
            var map = GetMapFromInput(input);

            var bufferMap = new char[map.Length][];
            for (int i = 0; i < bufferMap.Length; i++)
                bufferMap[i] = new char[map[i].Length];

            int seatedPersonsCount = 0;

            while (true)
            {
                AdvanceMap2(ref map, ref bufferMap);
                var count = CountCharOnMap('#', map);

                if (seatedPersonsCount == count)
                    break;

                seatedPersonsCount = count;
            }

            return seatedPersonsCount;
        }

        public static readonly string[] PUZZLE_INPUT =
        {
            "LLLLLLLLLLLL.LLLLLLL.LLLLLLL..LLLL.LLLLL.LLLLL.LLL..LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLL.L.LLL.LLLLL.LLLL..LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.L.LLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL..LLLLL.",
            "L.LLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LL.L.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL..LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LL.LLLL",
            "LLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LL.LLLLL.LLLL.LLLLLLLL.LLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLL",
            ".L...LL.L.L.L....L..L......LL...L..L.L.LLL.....L...L..LLL.L.LL..LLL......L..L.LLLLL.....L..L...",
            "LLLLL..LLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLL.LLLLLLLLLLLLLLLLLLLLL",
            "LLL.LLLLLLLL..LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.L.LLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.L.LLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLL..LLLLLLL.LLLLLLLLLL.LL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLL.LLLLLLL.LLL.LLLL.LLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLL...LLLLLL",
            "LLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLLL.LLLLLL.LLLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLL.LL.LL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLL..LLLLLLLLLLLLL.LLLLL.L",
            "L.LLLLLLLLLL.LLLLLLLLL.LLLLL.L.LLL.LLLLL.LLLLLLLLLL.LLLLLLLL.LLLL.LLLLL.L.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LL.LL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLL.LLLL.L.LLLLLL.LLLLLLL",
            "L...L.LL..L.L.LL.LL.L.....L..L.L.......L.L.LL...L....L......L.L.L.L.L..L....LL......L......L...",
            "LLLL..LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.L.LLLL.LLLL.LLLLL..LLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLL.LL.LL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.",
            "L..LL.LLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LL.L.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL..LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLL.LLLLL.L.LLLLL.LLLLLLL",
            "LL.LL.LLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLL.LLL",
            "...............L.L..LL.....L.L..L...LL.......LL....L......L...L....L...............LLL...L.LLL.",
            "LLLLLLLLLLL..LLLLLL.LLLLLLLL.LLLLL.LLLLL.LLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.L.LLLLL",
            "LL.LL.LLLLLL.LLLLLLLLL.LLLLL.LLLL..LLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLL.LLL.LLLLLLLLL.LLLLLLL",
            ".LLLL.LLLLLLL.LLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLL.LLL.LLLLLLLLLLLL.LLLLL.LLLLLLLLLL",
            "LLLLL.L.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLL..LLLLLLLLLLLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL",
            "LLL.L.LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "....L..L..........L......L.LL..L.L..LL.......L.....LL..L..LL...L......L....L..L..L....L.......L",
            ".L.LLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLL.LLLLLLLLLL.LL.LLLLLLLLLL.L.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLL.LL.LLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLLL.LLLL.L.LLLLLLLLLL..LLLLLLLLLLLL.LLLLLLLL.LLLLLLL",
            "LL.LL.LLLLLL.LLLL.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.L.LLLLLLLL.LLLLLLLLLLLL.L.LL.LLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLL..LLLLLLL",
            ".....L.L..LL.LLL.LL.L.L.....L..L.L......L......L.LL...L....LL..L.........L..L.....L.L....L..L..",
            "LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLL..LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLLLLLLLL.L.LLL.LLLLL.LL.LL.LLLL.L.LLLLL...LLL.LLL.LLLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.L.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL",
            "LLLLL.LLLLL..LLLLLLLLL.L.LLL.LLL.L.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.",
            "LLLLLLLLLLLL.LLLLLLLL..LLLLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLL.LLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLL",
            "...L..LL.L......L....LLL....L.L.LL..L.L..L.LL..L..L.L......L...L.L..L..LL....L..L..LL...LL.....",
            "L..LL.LLLLLL.LLLLLLLLLL.LL.L.LLLLL.L.LLL.LLLLLL.LLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLL..LL.LLLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLL.L.LLLLLLL.LLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL..LLLLLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLL.LLLL.L.LLL.LL.LLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLL..LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLL.L.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL",
            "LL..L.L.L.L..LL...LL....L.L..L..LLL..L.LL.L.LLLL..L.L.L..L...L.....L..LL........L...LL..L.L.LL.",
            "LLLLL.LLLL.L.LL.LLLLLL.LLLLLLLLLLLLL.LLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.L.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLL.LLLLLL.LLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LL.L.LLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL",
            "LLLLL.LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLL..LLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LL.L..L..L.....LL......L.......LLLLL..L.L.LL...L......L.L..L.L.L........L..L.L.L..LLLL....L....",
            "LLL.L.LLLLLLLLLLL.LLLL.LLLLL..LLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.L.LLL.LLLLLLL.LLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLL.LLLL..LLL.LLLLLL.LLLLLLLL.LLLL.LL.LLLLLLLLLLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLL..LLLLLLL.L.LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LL.LLLLL.LLLLLLL",
            "LLLLL.L.LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LL.LLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLL.L.LLLLL.L.LLL.LLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
            "LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLL.LLLLL.LL.L.LLL.LLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLL.",
            "LLLLL.LLLLLLLLLLLL.LLL.LLL.L.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL..LLLL.LLLLLLLL..LLLLLL",
            "LL.LL..L......L...L.....LL...L......L..L..LL...LL..L.L.....L.L...L...........LL...L.L.L..L.L.L.",
            "LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLL.LLLL.LLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLL.L.LLL..L.LLLLL.LLLLLLLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLL",
            "LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLL..LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL..LLLLLLL.LLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLL.LL.LLLL",
            "L....L..L..L...LL.L....L..LL.LL..L....L.L.....LL..........L...L...L..L.L.........L....L..L.....",
            "LLLLL.LLLL.LLLLL.LLLLL..LLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.L.LLLLLLLLLL.LLLLLLLLLLL.LLLL",
            "..LLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLL.L.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLL.L.LLLL.LLLLLLLLLLLLLLLL",
            "L.........LL.L..L.L....L.LLLL...L.L.....LLLL.........LL..LL....L...L..L.L...LL..LL...L.L.L..L.L",
            "LLLLL.LLLLLL..LLLLLLLL.LLL.L.LLLLL.LLL.L.LLLLL.L.LL.LLLLLLLL.LLLL.L.LLLLL...LL.LL.LLLLLLLLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLL",
            "LLLLLLLLLL.L.LLLLLLLLL.LLLLL.LLL.L.LLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLL.LL.L.LLLLLLLL.L.LLLLL",
            "LLLLL.LLLLLL.LLLLLLLLLLLLLL..LLLLL.LLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLL.LLLL.LLLL.LLL.LLLLLLL",
            "LLLLLLLLLLL..LLLLLLLLL.LLLLL.LLLLLLLLLLL.L.LLL.LLLL.LLLLLLLL.LLLL.LLLL.LL.LLL.LLLLLLLLLLLLLLLLL",
            "LLLLL.LLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLL..LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLL",
            ".LLLL.LLLLLL.LLLLLLLLL.LLLLL.L.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LL.L.LLLLLLL",
            "LLLLL.LLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.L.L..LLLLLLLLLLLLLLLL",
        };

    }
}

