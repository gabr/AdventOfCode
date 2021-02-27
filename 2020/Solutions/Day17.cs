using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day17
    {
        public record Cube(int[] Coordinates)
        {
            public Cube[] GetNeighbors()
            {
                var neighbors = new Cube[(int)Math.Pow(3, Coordinates.Length) - 1];
                var offsets = new int[Coordinates.Length];

                for (int i = 0; i < offsets.Length; i++)
                    offsets[i] = -1;

                for (int i = 0; i < neighbors.Length; i++)
                {
                    // advance offset
                    for (int oi = 0; oi < offsets.Length; oi++)
                    {
                        if (offsets[oi] == 1)
                        {
                            offsets[oi] = -1;
                        }
                        else
                        {
                            offsets[oi] += 1;
                            break;
                        }
                    }

                    // if got only zeros skip
                    bool allZeros = true;
                    for (int oi = 0; oi < offsets.Length; oi++)
                        if (offsets[oi] != 0)
                        {
                            allZeros = false;
                            break;
                        }

                    if (allZeros)
                    {
                        i--;
                        continue;
                    }

                    // produce neighbor
                    var newCubeCoordinates = new int[Coordinates.Length];
                    for (int oi = 0; oi < offsets.Length; oi++)
                        newCubeCoordinates[oi] = Coordinates[oi] + offsets[oi];

                    neighbors[i] = new Cube(newCubeCoordinates);
                }

                return neighbors;
            }

            public override string ToString() => string.Join(",", Coordinates);
        }

        public Dictionary<string, Cube> ParseInitialState(string[] initialState, int desiredDimension)
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
                        var coordinates = new int[desiredDimension];
                        for (int i = 3; i < coordinates.Length; i++)
                            coordinates[i] = 0;

                        coordinates[0] = x;
                        coordinates[1] = y;
                        coordinates[2] = z;

                        var cube = new Cube(coordinates);
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

        public int Solve(string[] initialState, int desiredDimension)
        {
            var activeCubes = ParseInitialState(initialState, desiredDimension);
            var buffer      = new Dictionary<string, Cube>();

            for (int i = 0; i < 6; i++)
                AdvanceState(ref activeCubes, ref buffer);

            return activeCubes.Count;
        }

        public int Solve1(string[] initialState) => Solve(initialState, 3);
        public int Solve2(string[] initialState) => Solve(initialState, 4);

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

