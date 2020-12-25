using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day17
    {
        public record Cube(int X, int Y, int Z)
        {
            public Cube[] GetNeighbors() => new Cube[]
            {
                new Cube(X-1, Y,   Z),
                new Cube(X-1, Y,   Z+1),
                new Cube(X-1, Y,   Z-1),
                new Cube(X-1, Y+1, Z),
                new Cube(X-1, Y+1, Z+1),
                new Cube(X-1, Y+1, Z-1),
                new Cube(X-1, Y-1, Z),
                new Cube(X-1, Y-1, Z+1),
                new Cube(X-1, Y-1, Z-1),

                new Cube(X+1, Y,   Z),
                new Cube(X+1, Y,   Z+1),
                new Cube(X+1, Y,   Z-1),
                new Cube(X+1, Y+1, Z),
                new Cube(X+1, Y+1, Z+1),
                new Cube(X+1, Y+1, Z-1),
                new Cube(X+1, Y-1, Z),
                new Cube(X+1, Y-1, Z+1),
                new Cube(X+1, Y-1, Z-1),

                new Cube(X, Y,   Z+1),
                new Cube(X, Y,   Z-1),
                new Cube(X, Y+1, Z),
                new Cube(X, Y+1, Z+1),
                new Cube(X, Y+1, Z-1),
                new Cube(X, Y-1, Z),
                new Cube(X, Y-1, Z+1),
                new Cube(X, Y-1, Z-1),
            };

            public override string ToString() => $"{X},{Y},{Z}";
        }

        public Dictionary<string, Cube> ParseInitialState(string[] initialState)
        {
            var activeCubes = new Dictionary<string, Cube>();

            int x, y, z;
            x = y = z = 0;

            foreach (var line in initialState)
            {
                x = 0;

                foreach (var cubeState in line.ToCharArray())
                {
                    if (cubeState == '#')
                    {
                        var cube = new Cube(x, y, z);
                        activeCubes[cube.ToString()] = cube;
                    }

                    x += 1;
                }

                y += 1;
            }

            return activeCubes;
        }

        public void AdvanceState(
            ref Dictionary<string, Cube> current,
            ref Dictionary<string, Cube> buffer)
        {
            buffer.Clear();

            foreach (var activeCube in current)
            {
                var cubeString = activeCube.Key;
                var cube       = activeCube.Value;

                var aliveNeighborsCount = 0;
                foreach (var neighbor in cube.GetNeighbors())
                {
                    if (current.ContainsKey(neighbor.ToString()))
                    {
                        aliveNeighborsCount += 1;
                    }
                    else
                    {
                        var aliveNCount = 0;
                        foreach (var n in neighbor.GetNeighbors())
                            if (current.ContainsKey(n.ToString()))
                                aliveNCount += 1;

                        if (aliveNCount == 3)
                            buffer[neighbor.ToString()] = neighbor;
                    }
                }

                if (aliveNeighborsCount == 2 || aliveNeighborsCount == 3)
                    buffer[cubeString] = cube;
            }

            var tmp = buffer;
            buffer = current;
            current = tmp;
        }

        public int Solve1(string[] initialState)
        {
            var activeCubes = ParseInitialState(initialState);
            var buffer      = new Dictionary<string, Cube>();

            for (int i = 0; i < 6; i++)
                AdvanceState(ref activeCubes, ref buffer);

            return activeCubes.Count;
        }

        public static readonly string[] PUZZLE_INPUT =
        {
            ".#.####.",
            ".#...##.",
            "..###.##",
            "#..#.#.#",
            "#..#....",
            "#.####..",
            "##.##..#",
            "#.#.#..#",
        };

    }
}

