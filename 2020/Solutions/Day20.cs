using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day20
    {
        public class Tile
        {
            private enum Flip : int
            {
                NotFliped = 0,
                Fliped    = 1
            }

            private enum Edge : int
            {
                Top    = 0,
                Right  = 1,
                Bottom = 2,
                Left   = 3,
            }

            public static readonly int NUMBER_OF_FLIPS = Enum.GetValues<Flip>().Length;
            public const int NUMBER_OF_ROTATIONS = 4;

            public static readonly int NUMBER_OF_POSSIBLE_TRANSFORMATIONS = NUMBER_OF_ROTATIONS * NUMBER_OF_FLIPS;

            // stored by lines, so [rowIndex][columnIndex]
            private char[][]  _tile;
            private int[][][] _possibleEdges;

            public UInt64 Id       { get; private set; }
            public bool   Fliped   { get; private set; } = false;
            public int    Rotation { get; private set; } = 0;
            public bool   Used     { get;         set; } = false;

            // edge is represented as a number after conversion: ###.#..# -> 11101001 -> 233
            public int TopEdge    => _possibleEdges[(int)(Fliped ? Flip.Fliped : Flip.NotFliped)][Rotation][(int)(Edge.Top)];
            public int RightEdge  => _possibleEdges[(int)(Fliped ? Flip.Fliped : Flip.NotFliped)][Rotation][(int)(Edge.Right)];
            public int BottomEdge => _possibleEdges[(int)(Fliped ? Flip.Fliped : Flip.NotFliped)][Rotation][(int)(Edge.Bottom)];
            public int LeftEdge   => _possibleEdges[(int)(Fliped ? Flip.Fliped : Flip.NotFliped)][Rotation][(int)(Edge.Left)];

            public int ImageSize  => _tile[0].Length - 2;

            public Tile(string[] tileLines)
            {
                _tile = tileLines
                    .Skip(1)
                    .Select(l => l.ToCharArray())
                    .ToArray();

                string idLine = tileLines[0];
                Id = UInt64.Parse(idLine.Substring(5, idLine.Length - 6));

                _possibleEdges = new int[Enum.GetValues<Flip>().Length][][];
                for (int i = 0; i < _possibleEdges.Length; i++)
                {
                    _possibleEdges[i] = new int[Enum.GetValues<Edge>().Length][];
                    for (int j = 0; j < _possibleEdges[i].Length; j++)
                        _possibleEdges[i][j] = new int[NUMBER_OF_ROTATIONS];
                }

                int edgeLength = _tile[0].Length;
                var top    = new char[edgeLength];
                var right  = new char[edgeLength];
                var bottom = new char[edgeLength];
                var left   = new char[edgeLength];

                for (int i = 0; i < edgeLength; i++)
                {
                    top[i]    = _tile[0][i];
                    right[i]  = _tile[i][edgeLength -1];
                    bottom[i] = _tile[edgeLength -1][i];
                    left[i]   = _tile[i][0];
                }

                int ConvertEdgeToNumber(string edge) => Convert.ToInt32((edge.Replace("#", "1").Replace(".", "0")), 2);

                var tmp = new char[edgeLength];
                for (int ri = 0; ri < NUMBER_OF_ROTATIONS; ri++)
                {
                    _possibleEdges[(int)(Flip.NotFliped)][ri][(int)(Edge.Top)]    = ConvertEdgeToNumber(new String(top));
                    _possibleEdges[(int)(Flip.NotFliped)][ri][(int)(Edge.Right)]  = ConvertEdgeToNumber(new String(right));
                    _possibleEdges[(int)(Flip.NotFliped)][ri][(int)(Edge.Bottom)] = ConvertEdgeToNumber(new String(bottom));
                    _possibleEdges[(int)(Flip.NotFliped)][ri][(int)(Edge.Left)]   = ConvertEdgeToNumber(new String(left));

                    // rotate
                    tmp    = left;
                    left   = bottom;
                    bottom = right;
                    right  = top;
                    top    = tmp;

                    Array.Reverse(top);
                    Array.Reverse(bottom);
                }

                // flip
                Array.Reverse(right);
                Array.Reverse(left);
                tmp    = bottom;
                bottom = top;
                top    = tmp;

                for (int ri = 0; ri < NUMBER_OF_ROTATIONS; ri++)
                {
                    _possibleEdges[(int)(Flip.Fliped)][ri][(int)(Edge.Top)]    = ConvertEdgeToNumber(new String(top));
                    _possibleEdges[(int)(Flip.Fliped)][ri][(int)(Edge.Right)]  = ConvertEdgeToNumber(new String(right));
                    _possibleEdges[(int)(Flip.Fliped)][ri][(int)(Edge.Bottom)] = ConvertEdgeToNumber(new String(bottom));
                    _possibleEdges[(int)(Flip.Fliped)][ri][(int)(Edge.Left)]   = ConvertEdgeToNumber(new String(left));

                    // rotate
                    tmp    = left;
                    left   = bottom;
                    bottom = right;
                    right  = top;
                    top    = tmp;

                    Array.Reverse(top);
                    Array.Reverse(bottom);
                }
            }

            // returns image as chars array [x][y] -> [columnIndex][rowIndex]
            public char[][] GetTileImageAccordingToCurrentTransformation()
            {
                // copy without edges and switch from storing data
                // per row to store them per column
                var result = new char[ImageSize][];
                for (int ci = 0; ci < ImageSize; ci++)
                    result[ci] = new char[ImageSize];

                for (int ci = 0; ci < ImageSize; ci++)
                    for (int ri = 0; ri < ImageSize; ri++)
                        result[ci][ri] = _tile[ri+1][ci+1];

                // transform
                if (Fliped)
                {
                    foreach (var column in result)
                        Array.Reverse(column);
                }

                if (Rotation == 0)
                    return result;

                if (Rotation == 1 || Rotation == 3)
                {
                    var copy = new char[ImageSize][];
                    for (int ci = 0; ci < ImageSize; ci++)
                        copy[ci] = new char[ImageSize];

                    for (int ri = 0; ri < ImageSize; ri++)
                    for (int ci = 0; ci < ImageSize; ci++)
                        copy[(ImageSize-ri)-1][ci] = result[ci][ri];

                    result = copy;
                }

                if (Rotation == 2 || Rotation == 3)
                {
                    Array.Reverse(result);
                    foreach (var column in result)
                        Array.Reverse(column);
                    return result;
                }

                return result;
            }

            public void SwitchToNextTransformation()
            {
                Rotation += 1;

                if (Rotation == NUMBER_OF_ROTATIONS) {
                    Rotation = 0;
                    Fliped = !Fliped;
                }
            }

            public override string ToString()
            {
                return $"{Id}, Fliped: {Fliped}, Rotation: {Rotation}";
            }

            public static Tile[] ParseTiles(string[] tilesLine)
            {
                var tiles       = new List<Tile>(tilesLine.Length / 11);
                var tileStrings = new List<string>(12);

                foreach (var line in tilesLine)
                {
                    if (line == "")
                    {
                        tiles.Add(new Tile(tileStrings.ToArray()));
                        tileStrings.Clear();
                    }
                    else
                    {
                        tileStrings.Add(line);
                    }
                }

                if (tileStrings.Count > 0)
                    tiles.Add(new Tile(tileStrings.ToArray()));

                return tiles.ToArray();
            }

        }

        public static Tile[][] AssembleTiles(Tile[] tiles)
        {
            // square image
            int width = (int)Math.Sqrt(tiles.Length);
            int height = width;

            // start assembling the tiles together
            var tilesMap = new Tile[width][];
            for (int w = 0; w < tilesMap.Length; w++)
                tilesMap[w] = new Tile[height];

            bool TileMatchesNeighbours(Tile tile, int w, int h)
            {
                return
                    (w <= 0 || tilesMap[w-1][h  ].RightEdge  == tile.LeftEdge) &&
                    (h <= 0 || tilesMap[w  ][h-1].BottomEdge == tile.TopEdge);
            }

            bool FindNextTile(int w, int h, string indent)
            {
                if (h == height)
                    return true;

                int nextW = w + 1;
                int nextH = h;

                if (nextW == width)
                {
                    nextW = 0;
                    nextH += 1;
                }

                foreach (var tile in tiles)
                {
                    if (tile.Used == true)
                        continue;

                    tile.Used = true;
                    tilesMap[w][h] = tile;

                    for (int i = 0; i < Tile.NUMBER_OF_POSSIBLE_TRANSFORMATIONS; i++)
                    {
                        if (TileMatchesNeighbours(tile, w, h))
                        {
                            if (FindNextTile(nextW, nextH, indent + " "))
                                return true;
                        }

                        tile.SwitchToNextTransformation();
                    }

                    tile.Used = false;
                    tilesMap[w][h] = null;
                }

                return false;
            }

            bool found = FindNextTile(0, 0, "");
            if (found == false)
                throw new Exception("Tiles filed to assemble");

            Console.WriteLine("FOUND:");
            for (int h = 0; h < height; h++)
            {
                for (int w = 0; w < width; w++)
                    Console.Write($"{tilesMap[w][h].Id}  ");
                Console.WriteLine();
            }

            return tilesMap;
        }

        public static char[][] ConvertTilesMapToImage(Tile[][] tilesMap)
        {
            // check assumption that tiles map is square
            foreach (var tilesLine in tilesMap)
                System.Diagnostics.Debug.Assert(tilesLine.Length == tilesMap.Length);

            var width = tilesMap.Length;
            var height = width;

            var tileImageSize = tilesMap[0][0].ImageSize;

            var image = new char[width*tileImageSize][];
            for (int i = 0; i < image.Length; i++)
                image[i] = new char[image.Length];

            for (int wi = 0; wi < width; wi++)
            for (int hi = 0; hi < height; hi++)
            {
                var tile = tilesMap[wi][hi];
                var tileImage = tile.GetTileImageAccordingToCurrentTransformation();

                /*
                Console.WriteLine(tile.Id);
                for (int ri = 0; ri < tileImageSize; ri++)
                {
                    for (int ci = 0; ci < tileImageSize; ci++)
                        Console.Write(tileImage[ci][ri]);
                    Console.WriteLine();
                }
                Console.WriteLine();
                */

                for (int ci = 0; ci < tileImageSize; ci++)
                for (int ri = 0; ri < tileImageSize; ri++)
                    image[(tileImageSize*wi)+ci][(tileImageSize*hi)+ri] = tileImage[ci][ri];
            }

            return image;
        }

        public static char[][] MarkPatternInImage(char[][] image, string[] pattern, char markChar)
        {
            // check assumption that all pattern lines are the same length
            foreach (var patternLine in pattern)
                System.Diagnostics.Debug.Assert(patternLine.Length == pattern[0].Length);

            // check assumption that all image rows are the same length and that the image is squared
            foreach (var imageColumn in image)
                System.Diagnostics.Debug.Assert(imageColumn.Length == image.Length);

            var imageSize     = image.Length;
            var patternWidth  = pattern[0].Length;
            var patternHeight = pattern.Length;

            bool found = false;
            void MarkIfMatches(int w, int h)
            {
                // does the pattern fit
                if (w+patternWidth-1  >= imageSize ||
                    h+patternHeight-1 >= imageSize)
                  return;

                for (int wi = 0; wi < patternWidth; wi++)
                for (int hi = 0; hi < patternHeight; hi++)
                    if (pattern[hi][wi] == '#' && image[w+wi][h+hi] != pattern[hi][wi])
                        return;

                found = true;

                for (int wi = 0; wi < patternWidth; wi++)
                for (int hi = 0; hi < patternHeight; hi++)
                    if (pattern[hi][wi] == '#' && image[w+wi][h+hi] == pattern[hi][wi])
                        image[w+wi][h+hi] = markChar;
            }

            for (int fi = 0; fi < Tile.NUMBER_OF_FLIPS; fi++)
            {
                for (int rc = 0; rc < Tile.NUMBER_OF_ROTATIONS; rc++)
                {
                    for (int wi = 0; wi < imageSize - patternWidth + 1; wi++)
                    for (int hi = 0; hi < imageSize - patternHeight + 1; hi++)
                        MarkIfMatches(wi, hi);

                    if (found)
                        return image;

                    // rotation
                    var copy = new char[imageSize][];
                    for (int ci = 0; ci < imageSize; ci++)
                        copy[ci] = new char[imageSize];

                    for (int ri = 0; ri < imageSize; ri++)
                    for (int ci = 0; ci < imageSize; ci++)
                        copy[(imageSize-ri)-1][ci] = image[ci][ri];

                    image = copy;
                }

                foreach (var imageColumn in image)
                    Array.Reverse(imageColumn);
            }

            return null;
        }

        public UInt64 Solve1(string[] tilesStrings)
        {
            var tiles = Tile.ParseTiles(tilesStrings);
            var assembledTiles = AssembleTiles(tiles);

            var maxIndex = assembledTiles[0].Length-1;

            UInt64 sum = 1ul;

            sum *= (UInt64)assembledTiles[0       ][0       ].Id;
            sum *= (UInt64)assembledTiles[0       ][maxIndex].Id;
            sum *= (UInt64)assembledTiles[maxIndex][0       ].Id;
            sum *= (UInt64)assembledTiles[maxIndex][maxIndex].Id;

            return sum;
        }

        public UInt64 Solve2(string[] tilesStrings)
        {
            var tiles          = Tile.ParseTiles(tilesStrings);
            var assembledTiles = AssembleTiles(tiles);
            var image          = ConvertTilesMapToImage(assembledTiles);
            var markedImage    = MarkPatternInImage(image, SEA_MONSTER_PATTERN, 'O');

            if (markedImage == null)
                throw new Exception("Pattern not found");

            UInt64 remaingWatterCount = 0ul;

            var size = markedImage.Length;
            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    Console.Write(markedImage[j][i]);
                    if (markedImage[i][j] == '#')
                        remaingWatterCount += 1ul;
                }
                Console.WriteLine();
            }

            return remaingWatterCount;
        }

        public static readonly string[] SEA_MONSTER_PATTERN =
        {
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   ",
        };

        public static readonly string[] PUZZLE_INPUT =
        {
            "Tile 3041:",
            "#..##.#...",
            "..#....###",
            ".#..#.#.##",
            "..#.......",
            "#.#..###.#",
            ".#....#..#",
            ".........#",
            "#...##.###",
            "#.....###.",
            "...##..#..",
            "",
            "Tile 1747:",
            "##..######",
            "#####...##",
            "..#.....##",
            "#...##.###",
            "#........#",
            "###.#..#.#",
            "#.##...#..",
            "#..#.....#",
            "#.#..#...#",
            "#.#.#..###",
            "",
            "Tile 2887:",
            "##....#..#",
            "##....#...",
            "#...#..#..",
            "...###.#..",
            ".....#.##.",
            ".....#....",
            "##.##....#",
            "#.......#.",
            "###....##.",
            "##..##.#.#",
            "",
            "Tile 3001:",
            "##.#.#.#..",
            "##...####.",
            "##....#.##",
            "....##....",
            ".......##.",
            "#..#..#...",
            "###..#.#.#",
            "###....###",
            "####...###",
            "##..##.#.#",
            "",
            "Tile 2267:",
            "##.#.#.###",
            "##....#..#",
            "#..#....#.",
            "..#.##..#.",
            "##..#..#..",
            "...#......",
            "#.#.#.....",
            "##........",
            "...#.#..##",
            "..##.#..#.",
            "",
            "Tile 2797:",
            "#.##.#...#",
            ".......#..",
            "#....#....",
            "##..##.#.#",
            "#......##.",
            ".#.##.#.##",
            "#......##.",
            "####....#.",
            "#.##....#.",
            "####.#.###",
            "",
            "Tile 1087:",
            "#.#######.",
            "##..##..#.",
            "#.......##",
            ".....#....",
            "##...#...#",
            "#....#...#",
            ".##......#",
            ".#..#.#..#",
            "#...#..##.",
            ".##..##.##",
            "",
            "Tile 2237:",
            ".#..####..",
            "...####.##",
            "##...#..#.",
            "#..#.#...#",
            "##.#......",
            ".##.#..#.#",
            ".#..##...#",
            "...#....##",
            ".....##...",
            ".....##..#",
            "",
            "Tile 3673:",
            "##.###...#",
            "#.##..#..#",
            "#..####...",
            ".#.####...",
            ".....#.#.#",
            "#..#..#...",
            "###.......",
            "#.#.......",
            "#.......#.",
            "##..###..#",
            "",
            "Tile 1907:",
            "...#.#.###",
            "#.##.....#",
            ".......##.",
            ".##.#...#.",
            "..#.##....",
            "##..##....",
            "#.........",
            "##.#......",
            ".......#..",
            "#.#.###...",
            "",
            "Tile 2437:",
            "#.##.##..#",
            "....#.....",
            "..#......#",
            "#...#.....",
            "...#......",
            ".....#...#",
            "#.#.......",
            ".#...#...#",
            "#.#....#..",
            ".##..##...",
            "",
            "Tile 2897:",
            "##..#...#.",
            "..###....#",
            "...#.##...",
            "....#.##..",
            "...##.....",
            "#..#..#.##",
            "..###.....",
            "...##..##.",
            "#..#......",
            "#.#.##..#.",
            "",
            "Tile 1103:",
            ".####.#..#",
            "..##...#..",
            "#.....#...",
            "##.#.....#",
            "#.##......",
            ".##...#..#",
            "##.....#.#",
            "##.....###",
            "#.....#...",
            "##.#...##.",
            "",
            "Tile 1559:",
            "...##..#.#",
            "#..###...#",
            "...##...##",
            "#..#.#.#..",
            "#......#.#",
            "#..#.#.#.#",
            ".#..#....#",
            "....#..###",
            "..##..##..",
            ".#..##..##",
            "",
            "Tile 3931:",
            "##....##.#",
            ".##..#...#",
            ".......#..",
            "#....#.#..",
            "#.#..##.#.",
            ".##.####..",
            ".......#.#",
            ".....#.#..",
            ".#.#......",
            "####..#.##",
            "",
            "Tile 3331:",
            "#.#.####..",
            ".#.##...##",
            "....####.#",
            ".##......#",
            "#.##.##..#",
            "##..#...#.",
            "#.##....#.",
            "#.#.#.#.#.",
            "...#......",
            "###.##.##.",
            "",
            "Tile 2089:",
            "..########",
            "##.#...##.",
            "#...#.....",
            "##......##",
            "##..##..#.",
            "..#.#....#",
            "#..##....#",
            "#..#.#...#",
            "##..#....#",
            "..######.#",
            "",
            "Tile 1069:",
            ".####..###",
            "......##..",
            "....#.##.#",
            "#..#.....#",
            "#...#...#.",
            "#...#.##.#",
            "...#.#...#",
            "#.#.......",
            "#.#.##.#.#",
            "#..#.#.#..",
            "",
            "Tile 1831:",
            "#..#..##..",
            "#...##...#",
            "..##...#.#",
            "#.......##",
            "#........#",
            "..#.......",
            "##..###..#",
            "#....###.#",
            "...#......",
            "##..#.....",
            "",
            "Tile 1453:",
            ".#.#.#..##",
            "##.##...##",
            "#....#....",
            "#..#..#.#.",
            "##.#..###.",
            "...##.##..",
            "....##..##",
            "...##..###",
            "##........",
            "..##.#####",
            "",
            "Tile 3767:",
            "..#.#.#.#.",
            "..........",
            "#....#.#.#",
            "#.....#..#",
            "##..#..#..",
            "...#..#..#",
            "..#....#..",
            "......#...",
            "#..##.....",
            ".#....##.#",
            "",
            "Tile 1693:",
            "#.###..###",
            "#...#...##",
            "#......###",
            "#....#...#",
            "......#..#",
            "###...#...",
            "####..##..",
            "...#..#...",
            "##..##.###",
            "#....#.##.",
            "",
            "Tile 3631:",
            "....#.##..",
            "..#...#...",
            "#.###..#..",
            "....#.#..#",
            "#..#..##..",
            "#....#.#..",
            "#.##..#...",
            "#........#",
            ".....#...#",
            "...#......",
            "",
            "Tile 2999:",
            "#...###..#",
            "#.#.##....",
            "#.........",
            "##.......#",
            "#.#.##.###",
            ".....##.##",
            ".#.##..##.",
            "#...###.##",
            ".#.....#..",
            "..#.#.###.",
            "",
            "Tile 2417:",
            "..#.#.#...",
            "##.#..#.##",
            "#.##....#.",
            "#.........",
            "...#..#...",
            ".#...#.###",
            ".#.###...#",
            "#........#",
            "#.#..#....",
            "#.##.#...#",
            "",
            "Tile 3919:",
            "##........",
            "#.#.......",
            ".#.#.#....",
            "...#...#.#",
            "...#...##.",
            "##.....###",
            "#.#.#.#...",
            "###.#.....",
            ".#.#..#.##",
            "##.#...##.",
            "",
            "Tile 3557:",
            "##.#.###..",
            ".##...#..#",
            ".#.#...#..",
            "....###...",
            ".....#####",
            "###.#....#",
            ".#..##..#.",
            "#..#....#.",
            "..........",
            "....###.#.",
            "",
            "Tile 3851:",
            ".##.###.##",
            "..#......#",
            "......#..#",
            "#.#.......",
            "......##..",
            "##...#....",
            "..#..#...#",
            "...#.#....",
            ".........#",
            ".#.##..###",
            "",
            "Tile 1013:",
            "##.##.##..",
            "##........",
            "...#......",
            "#.....#...",
            "#.#..#...#",
            ".#......##",
            "...##.#.##",
            "..#..##..#",
            ".#...#..#.",
            "..#..###.#",
            "",
            "Tile 1549:",
            "..#...##.#",
            "..#......#",
            "##....#.#.",
            ".......##.",
            "..#...#..#",
            "#.#..#..##",
            ".......#.#",
            ".......#.#",
            "#...##...#",
            ".######...",
            "",
            "Tile 2179:",
            ".##.##.#..",
            ".##.###...",
            ".........#",
            "..#..#..#.",
            "..#####...",
            "...#..#..#",
            "...#....##",
            "#........#",
            "#.........",
            "###.###...",
            "",
            "Tile 1697:",
            "###..###.#",
            "##...#....",
            "####...#.#",
            ".#.....##.",
            "....####.#",
            "#....##.##",
            "..#.##...#",
            ".........#",
            "###..##.##",
            "#..##.####",
            "",
            "Tile 1307:",
            "##......#.",
            "..........",
            "....#....#",
            "#...#.#.#.",
            ".###.....#",
            "#..##.#..#",
            "..........",
            ".....##.#.",
            "...####..#",
            "..#..#.#..",
            "",
            "Tile 3929:",
            "###.#.##..",
            "##........",
            "###....###",
            "...#..#..#",
            "...#.....#",
            "..#.###...",
            "..........",
            ".....#...#",
            "#.......##",
            "##....#.#.",
            "",
            "Tile 1949:",
            "..####.#.#",
            ".#..#...#.",
            "#..#....##",
            "#.....###.",
            ".........#",
            "..#..#...#",
            ".....##...",
            ".....#....",
            ".#.#.#.#..",
            "####.#####",
            "",
            "Tile 2081:",
            "##.##.####",
            ".#........",
            "#...#.....",
            ".#.##..#.#",
            "....###..#",
            "..#..#.#..",
            "#.#.#.##.#",
            "..#..#..#.",
            "#...#.#...",
            ".##.#.####",
            "",
            "Tile 1433:",
            "#####.##..",
            "#.....#...",
            "####..#...",
            "...####...",
            "......#.##",
            "..##...#..",
            "#...#.#.#.",
            "#....#..#.",
            "#......#.#",
            "##..###..#",
            "",
            "Tile 3433:",
            "#.......##",
            "#....#....",
            "#...#.#..#",
            "..#.##...#",
            "#...##.#.#",
            "##.####..#",
            "..##.###.#",
            "#.#....#..",
            "##.#.....#",
            ".#....#..#",
            "",
            "Tile 3607:",
            "##.#.#.#.#",
            "...#..#.#.",
            "#.#..#.#..",
            "#.......##",
            "..#..##...",
            "#.....#...",
            "#...#..#.#",
            "...#....##",
            "#..#..#.##",
            "##.#..#.#.",
            "",
            "Tile 2677:",
            ".###..#..#",
            "###......#",
            "..........",
            "##..#...#.",
            "##...#.#..",
            "..#.......",
            "#...#...#.",
            "#......#.#",
            "##.#.#.#.#",
            "###...#...",
            "",
            "Tile 3461:",
            "...##...##",
            "#.#...##.#",
            "#.....##..",
            "#..#....##",
            "#.#....#.#",
            "..##.##.#.",
            "#.........",
            "..#.#....#",
            "#....##...",
            ".#.##.####",
            "",
            "Tile 3911:",
            "#.#####.##",
            "#..#.....#",
            "#..#......",
            ".##....##.",
            "#..#..#..#",
            ".....#.#.#",
            "#.......##",
            "......#...",
            "####...##.",
            "..#..#.#..",
            "",
            "Tile 2729:",
            "##..##.##.",
            ".#..#...#.",
            "#....###..",
            ".....##...",
            "#..#..#..#",
            "....###..#",
            "#....##.##",
            "#..#.###..",
            "#.....#...",
            "...##.#...",
            "",
            "Tile 2549:",
            ".#.##.#...",
            "#..#.#.#.#",
            ".........#",
            ".#..##..##",
            "#....##...",
            "##..#.##.#",
            ".#...##...",
            "#...#.....",
            "..#.......",
            "#....#....",
            "",
            "Tile 1699:",
            ".######..#",
            "....###..#",
            ".####..#..",
            "#....#....",
            ".##.#...#.",
            "#.......#.",
            ".#.#.##..#",
            "#..#......",
            "..........",
            "#.##..##..",
            "",
            "Tile 2221:",
            "#...##.#..",
            "....#....#",
            "#...#....#",
            "#........#",
            "#.......##",
            "##.#..#...",
            "#.#.###..#",
            "#..#.....#",
            "......#.##",
            "..#.####.#",
            "",
            "Tile 2129:",
            "....#####.",
            ".##..#...#",
            "......#.##",
            "#.#...#...",
            "#..#...###",
            "###.#..#..",
            ".....#...#",
            "...#......",
            "#...#.#...",
            "#.###.###.",
            "",
            "Tile 1973:",
            ".......###",
            "..........",
            "#.#.#.....",
            ".........#",
            "....#..#..",
            "...#...#..",
            "##.....#.#",
            "#..#...#.#",
            "#.#......#",
            "#...#.#.##",
            "",
            "Tile 3943:",
            "...###....",
            "........##",
            ".##.......",
            "##.#....##",
            "..#.#..#..",
            "..##...#..",
            "#.##....#.",
            ".#####.#..",
            "###..##.#.",
            "...#..#.##",
            "",
            "Tile 1451:",
            "#..###..#.",
            "#..#....##",
            "....#...##",
            "##.......#",
            "#.........",
            "..........",
            "...#...#..",
            "..#....##.",
            "...#..#.#.",
            ".##...##.#",
            "",
            "Tile 1759:",
            ".#..##...#",
            "##........",
            "##..#.....",
            "..........",
            ".###...###",
            "..##..#..#",
            ".......###",
            "#..#..#..#",
            ".##....#..",
            "...##.#..#",
            "",
            "Tile 2963:",
            "##.##.#.#.",
            "....#.....",
            "#.....##.#",
            ".##.#..#.#",
            ".###..#.#.",
            "#.##.##...",
            "....#....#",
            "#.....#...",
            ".#.##.###.",
            ".#..#.....",
            "",
            "Tile 3313:",
            "#.#...#...",
            "..##..#..#",
            "#####.##.#",
            "#.....##.#",
            "..#..##..#",
            "##.###....",
            "...#.#....",
            ".......#.#",
            "##.....##.",
            "#.#.#####.",
            "",
            "Tile 3221:",
            ".#...##...",
            "#.##...###",
            "####...##.",
            ".##.......",
            "##..#...#.",
            "...#.#.###",
            "#...##..#.",
            "##...#..#.",
            "#.#...##..",
            "##...#.##.",
            "",
            "Tile 3989:",
            "###...#.##",
            "#.......##",
            "#.......#.",
            "#...#....#",
            "#.........",
            "...#.##...",
            "..#....#..",
            "..........",
            ".#......#.",
            "#.##.###..",
            "",
            "Tile 2069:",
            "#..#..##.#",
            "#.####....",
            "#..#......",
            "#..##..#.#",
            "#......###",
            "#...#.....",
            "#...#..###",
            "......##..",
            "#..#.####.",
            "#.#..#..#.",
            "",
            "Tile 1871:",
            "#.##..####",
            ".#.##....#",
            "###.#.#..#",
            "#....#...#",
            "......#.##",
            "#.....####",
            "#.....##.#",
            "#...#....#",
            "....#.....",
            ".......#.#",
            "",
            "Tile 2083:",
            ".#.##.#...",
            "#..####..#",
            "##.#.##.##",
            "..#.##....",
            "#.#..##...",
            "..#.#..#..",
            "...#..#...",
            ".#..#.##..",
            "###..#.###",
            "#...#.#...",
            "",
            "Tile 2309:",
            ".##.#...#.",
            "#..##..#.#",
            "...#......",
            ".#..#.....",
            "#..##...##",
            ".#...#...#",
            "...#.#....",
            ".....##...",
            "#.##....##",
            "...##..###",
            "",
            "Tile 3413:",
            ".....####.",
            "..#.###...",
            "..#.#....#",
            "##.##.....",
            "..##...###",
            "##..#.....",
            "##.#.....#",
            "###.######",
            "#..##.....",
            "#.#...#.#.",
            "",
            "Tile 1039:",
            "#.##.#.#..",
            "#...#.#...",
            "...####.#.",
            "..#..#.#.#",
            "...#.##..#",
            "#...###..#",
            "....#..#..",
            "##....##.#",
            ".#...#....",
            "#...#.#..#",
            "",
            "Tile 2029:",
            "##..##.#..",
            "#.........",
            "#..#......",
            ".##.......",
            ".....#..#.",
            ".##..##...",
            "......#...",
            "#....#.#.#",
            "..##.#....",
            "...#.#.#..",
            "",
            "Tile 2281:",
            "######.##.",
            "#...##....",
            "......#.#.",
            "##..#.##..",
            "#..####.##",
            "#....#.#.#",
            "#........#",
            "........##",
            ".##....##.",
            "..###.....",
            "",
            "Tile 3259:",
            "#...##.#.#",
            "...#....#.",
            "..#.#...#.",
            "###.......",
            ".####.#.##",
            "#..#...#.#",
            "#....#.##.",
            "#..###..#.",
            "#.###..#.#",
            "#.#...###.",
            "",
            "Tile 2087:",
            "###.###.#.",
            "#.........",
            "#.......#.",
            "##....####",
            "#.##...#..",
            "#..##.#.##",
            ".##...#...",
            ".##..#..#.",
            "#.....#...",
            "##..#.#...",
            "",
            "Tile 3391:",
            ".##.#.#..#",
            "##.#.#...#",
            "#..#......",
            "...#......",
            "#..#......",
            "..........",
            "...###...#",
            ".........#",
            "##..#....#",
            "..#.#....#",
            "",
            "Tile 2693:",
            "#...#.##..",
            "#..##....#",
            ".#.#....#.",
            "#.#.#...#.",
            "#.##.....#",
            "...#......",
            "..#.#.##.#",
            "#.......#.",
            "...#.....#",
            "..###..##.",
            "",
            "Tile 1279:",
            "#.#.....##",
            ".........#",
            "#.#..#.#.#",
            "#..#.....#",
            "#..#......",
            "###.....##",
            ".###...#.#",
            "#.##.....#",
            "#.##......",
            ".#..####..",
            "",
            "Tile 2459:",
            "####...#.#",
            "#.#....#.#",
            "#........#",
            ".#........",
            "##......#.",
            "#...#.#..#",
            "###....#..",
            "..##.##..#",
            "#.#.....##",
            "#.#.#.....",
            "",
            "Tile 2297:",
            ".##.####..",
            "#...#...##",
            "##........",
            ".##...#.#.",
            ".....#.#.#",
            "#........#",
            "......#.#.",
            "#...#...##",
            "#.#.....##",
            ".##..##..#",
            "",
            "Tile 3217:",
            ".##..##..#",
            "#...#.#...",
            ".#.##.#...",
            "##.##....#",
            "#...##..##",
            "..###...#.",
            ".........#",
            "#..#.#....",
            ".#..#...##",
            "#....##..#",
            "",
            "Tile 2467:",
            "#.#.##.###",
            ".....#...#",
            ".#........",
            "#...##.###",
            ".#.......#",
            "#...#.##.#",
            "#....#....",
            "#...#....#",
            "...##.##.#",
            "....#..#.#",
            "",
            "Tile 2939:",
            "##.###.#..",
            "...#..#..#",
            ".#..##....",
            "#.#.......",
            "##....#...",
            ".##...##..",
            "##.#.....#",
            "#..#..#.##",
            "..........",
            "###..##.##",
            "",
            "Tile 1459:",
            "#.#.##.###",
            "..##.#...#",
            "#..#.#.##.",
            "#....###..",
            "##...#...#",
            ".##.#.....",
            ".##.#.##..",
            "#.....#..#",
            "#.#.##....",
            "..#....##.",
            "",
            "Tile 3583:",
            ".#..#.#.##",
            "#..###.#.#",
            ".#....#...",
            ".#.##....#",
            "...#.....#",
            "...#.#...#",
            "....###...",
            ".....#....",
            "..#.......",
            ".#.#.###..",
            "",
            "Tile 3209:",
            "#.#.######",
            ".#....#.#.",
            "..#.......",
            "#.#.....##",
            "#..#.....#",
            ".#.......#",
            "#.##.#....",
            "....##....",
            ".#.##.#.#.",
            "#...#.#...",
            "",
            "Tile 1471:",
            "#.###..##.",
            "........##",
            ".#....##..",
            "####.#...#",
            "...#......",
            "#....#....",
            "#.....#.#.",
            "#.#......#",
            "##.#.##..#",
            ".#.#.###.#",
            "",
            "Tile 3533:",
            "....#..#.#",
            "###...#...",
            "..#.#..#..",
            "...#..#...",
            "#.#.#..#.#",
            ".#..##..#.",
            "#..#.....#",
            ".....#.#.#",
            "........##",
            "..#.#.##.#",
            "",
            "Tile 2389:",
            ".....##...",
            "..#....#.#",
            "....#...#.",
            "#.....#...",
            "#..#....##",
            "#.#.#.....",
            "#........#",
            "#.........",
            "....#.#...",
            "....##..##",
            "",
            "Tile 3023:",
            "....#.###.",
            "#...#.#.##",
            "#.#.#.....",
            "#..#.#..##",
            "....#....#",
            "....######",
            "#...#.....",
            ".#....#.##",
            "#......#..",
            "#.##...##.",
            "",
            "Tile 1367:",
            ".#...###..",
            "#...##.##.",
            ".#.....#.#",
            ".#...#...#",
            "####.#....",
            ".##.......",
            ".#.....#..",
            "...#.....#",
            "..#......#",
            "###..#..#.",
            "",
            "Tile 1789:",
            ".##..#..##",
            "....#.##..",
            ".####.....",
            "#...###..#",
            "#..#..###.",
            ".##.#.....",
            "..#.##....",
            ".....#....",
            ".......#..",
            ".#..##....",
            "",
            "Tile 3011:",
            "#.###..#.#",
            ".....#...#",
            "...#..#..#",
            "#.###..#..",
            "#..#.#..##",
            "..##......",
            ".#...#...#",
            "...#..#...",
            ".#.###...#",
            ".#.##.....",
            "",
            "Tile 3547:",
            ".###.#...#",
            ".##....#..",
            "...#.##.##",
            "#.#.......",
            ".......#.#",
            "......#..#",
            "#..#...#..",
            "...#.####.",
            "#....##.#.",
            "....#.##..",
            "",
            "Tile 3671:",
            "##.#..#.#.",
            "##..##.#.#",
            "...###....",
            "#.#.###..#",
            "#..#...#..",
            "..#......#",
            "#.#...####",
            ".....#...#",
            ".#...##..#",
            "..###..#..",
            "",
            "Tile 3529:",
            "#.#.#..#.#",
            "##.....#..",
            ".##..#....",
            "#.#....###",
            "#.##.#..##",
            "#....#.#.#",
            ".....###.#",
            "....#..#.#",
            ".#..#....#",
            ".###..##..",
            "",
            "Tile 2251:",
            ".####.#.##",
            "#.....#...",
            ".#...#.#.#",
            "...##.....",
            "##.......#",
            ".....#....",
            "...##....#",
            "#..#..#...",
            "#########.",
            "#.........",
            "",
            "Tile 2927:",
            ".#...#####",
            ".###..#...",
            "...#...#..",
            "##.#...##.",
            "...#.....#",
            "#.###...##",
            ".....#..#.",
            "#....###.#",
            "#........#",
            ".##..##...",
            "",
            "Tile 3467:",
            "#...#.#.##",
            ".....#....",
            "...###..##",
            "#...#.#...",
            "...#.#.#..",
            "....#.....",
            ".....#.#..",
            "..#.#...#.",
            ".....##...",
            "#..#..#...",
            "",
            "Tile 2341:",
            "#.......##",
            "..##..#.#.",
            ".....##..#",
            "#..###.#..",
            "...##.....",
            "##..#..#.#",
            "###..#.#.#",
            ".#..#..###",
            "...#..##..",
            ".#.#...###",
            "",
            "Tile 1847:",
            "#..##..###",
            "#....#.#.#",
            ".#..###...",
            "#....#...#",
            "#.#..##..#",
            "##.####...",
            "#.#.......",
            "..........",
            ".#..#...##",
            "##..#..##.",
            "",
            "Tile 1597:",
            "#.#.#....#",
            "#......###",
            ".#..#....#",
            ".#....####",
            "#.#..#..##",
            "#...#..#.#",
            "#.##.##..#",
            "###...#...",
            "#..##.#...",
            ".###..#.#.",
            "",
            "Tile 2789:",
            ".#####.#..",
            "....#.....",
            ".........#",
            "#.##....#.",
            "..#.#.#...",
            "#...##....",
            "#..#...#.#",
            "#..#.....#",
            ".....#....",
            ".#..####.#",
            "",
            "Tile 2441:",
            "##.#.#....",
            "#.#.......",
            ".#...###..",
            "..#....##.",
            ".......#.#",
            ".......#..",
            ".#...##..#",
            "####......",
            "##..#....#",
            "##.##.###.",
            "",
            "Tile 1303:",
            ".##...#...",
            "#........#",
            "###....#.#",
            ".....#....",
            ".....#....",
            "#.....#..#",
            ".........#",
            ".####.#..#",
            "..##...###",
            "####..###.",
            "",
            "Tile 2339:",
            "..#..##.##",
            "...#....##",
            "##.#.#....",
            "#..##.##.#",
            ".####..#.#",
            "#.........",
            ".#...#..#.",
            "......#..#",
            "#..###...#",
            ".##..#.#.#",
            "",
            "Tile 3079:",
            "####...#.#",
            "...###..##",
            ".........#",
            ".##....#..",
            "##..#..#..",
            "...#..##.#",
            "#.#....#..",
            "##........",
            "#.#.####..",
            "###.#.#..#",
            "",
            "Tile 1877:",
            "..#.#.#..#",
            "#.#.#.#...",
            "#....##...",
            "..##.#..##",
            "##.#..#...",
            "#..#......",
            "#...#.#..#",
            ".....###.#",
            "..#..##..#",
            "########.#",
            "",
            "Tile 2663:",
            "##.##....#",
            "#......##.",
            "#.#...#..#",
            "##.###....",
            "##..#.#.##",
            "#...#..#..",
            "#..#....#.",
            "..#.#...##",
            "#..##...##",
            ".....###.#",
            "",
            "Tile 1109:",
            "###....##.",
            "#........#",
            "#.........",
            "...#..####",
            "..##..##.#",
            ".....##..#",
            ".#...#.#.#",
            "#...#...##",
            "..........",
            "...#.#.###",
            "",
            "Tile 2971:",
            "...##.####",
            "#..#....##",
            "#...#.#.#.",
            "#...#.##.#",
            "...#.....#",
            ".#........",
            "#.....#.#.",
            ".#####.#.#",
            "##..#...#.",
            "###.#..#.#",
            "",
            "Tile 2053:",
            ".###.###..",
            "####.#..##",
            "..#...##..",
            "##....##.#",
            "#.#..#.###",
            "..#.......",
            ".#.##...#.",
            "#...##..##",
            "..#...#..#",
            "#.#....###",
            "",
            "Tile 2647:",
            ".###.#.###",
            "#........#",
            "#....#..##",
            "........#.",
            "##....##.#",
            "##....##..",
            "....#..#.#",
            "...#.....#",
            "##.......#",
            "##.#...#.#",
            "",
            "Tile 1423:",
            "####.#....",
            "#.....#...",
            "#.#....#.#",
            "...#.....#",
            "..#.......",
            "#.#.......",
            "##..#....#",
            "##.##.#...",
            "#...#.....",
            "...#.##.##",
            "",
            "Tile 2767:",
            "..#.##..##",
            "#...#..#.#",
            "#.#....#..",
            "#.......#.",
            ".......#..",
            ".#...#.#.#",
            ".##.#..#..",
            ".....##..#",
            "#.......##",
            "##....####",
            "",
            "Tile 2203:",
            ".#...##.##",
            "#.##...#.#",
            ".#........",
            "...#.##..#",
            "###....###",
            ".#.....#.#",
            "#.#.##...#",
            "...#.#.#..",
            ".....#....",
            "#.####.##.",
            "",
            "Tile 1667:",
            "....#.##.#",
            "#..#...#..",
            ".#...#...#",
            ".#........",
            ".#........",
            "...#.....#",
            "..#..#...#",
            "##...####.",
            ".#.....#..",
            "#..#......",
            "",
            "Tile 2633:",
            ".#.#######",
            "#..###....",
            "........#.",
            "#.........",
            ".....##.#.",
            "#.#......#",
            "#.#.......",
            "..##.....#",
            "....##....",
            ".#..####.#",
            "",
            "Tile 3539:",
            "####.##..#",
            "..##....#.",
            "..#.#.##.#",
            "...#.....#",
            "#.........",
            ".......#.#",
            "...#....#.",
            "###..####.",
            ".##......#",
            "..#..#####",
            "",
            "Tile 2381:",
            ".######...",
            "##..###...",
            "#......#.#",
            "........##",
            ".#.....#.#",
            "##..##...#",
            ".#.......#",
            "#.........",
            "...##....#",
            "....##...#",
            "",
            "Tile 1153:",
            "..##.##..#",
            ".....##.#.",
            ".....##...",
            "..#.....##",
            "##.#..#...",
            "#.....#..#",
            "#....#..#.",
            "#....#.#..",
            "..#..#..#.",
            ".#.##.....",
            "",
            "Tile 2011:",
            "#.##..#...",
            "#.##.....#",
            "...#..#...",
            "###.......",
            "#....#..#.",
            "..##......",
            "##..#..#..",
            "..##....##",
            "#..#..#...",
            "##.####..#",
            "",
            "Tile 2731:",
            "........#.",
            "#........#",
            "##.#.....#",
            "#.....##..",
            "#.....#...",
            "#.#......#",
            "...#.###.#",
            "#.#.#....#",
            ".#.#...#.#",
            "#...###..#",
            "",
            "Tile 3643:",
            ".#...#.###",
            "..#...#..#",
            "#....##..#",
            "...##.#.#.",
            ".#.....#..",
            "...#.##..#",
            ".####.##..",
            "#..#..####",
            "....#....#",
            "####.####.",
            "",
            "Tile 2917:",
            "..########",
            "#......#.#",
            "#..#.#..##",
            "#...#...##",
            "#..##....#",
            "...#..#...",
            "..#......#",
            "...#...#.#",
            "#.....#..#",
            ".#.#..####",
            "",
            "Tile 1951:",
            "#.#.#..##.",
            "..........",
            "..#.#.#.##",
            "#.#.......",
            ".#..#...##",
            "...##..#.#",
            "#...#..#..",
            ".#..#.....",
            "#.#.....##",
            ".#####....",
            "",
            "Tile 2593:",
            "..#####.#.",
            ".#...#....",
            "##..###..#",
            "###.##..##",
            ".#....##.#",
            ".#..##....",
            "#.#..#..##",
            ".....#....",
            ".....#...#",
            ".####..#..",
            "",
            "Tile 2953:",
            "..#..#.#.#",
            "...#..#...",
            "##.....#.#",
            "......#..#",
            "#........#",
            "..#..#...#",
            "#.........",
            "###.#.##..",
            "##..#..#..",
            ".###.#..#.",
            "",
            "Tile 3769:",
            "......#...",
            ".#.##.#...",
            ".##......#",
            "##........",
            "#.....###.",
            "....#...#.",
            "..........",
            "#.....#.##",
            "..........",
            ".##..##.##",
            "",
            "Tile 2423:",
            "####..#.##",
            ".#..##....",
            "#.#.#.....",
            ".........#",
            "..........",
            "..#..#....",
            ".#.#..#..#",
            "#.........",
            "#..#..#...",
            "##.#......",
            "",
            "Tile 3329:",
            "..#####.#.",
            ".#....##.#",
            "###..#####",
            "##...#...#",
            "#.........",
            "#.........",
            "...#.#...#",
            "....#...##",
            "#...#.#...",
            "#.........",
            "",
            "Tile 1249:",
            "..#..###.#",
            "......#.#.",
            "##...#....",
            "..........",
            "##.......#",
            "#......#.#",
            "#..#.#..##",
            ".#.......#",
            "###....#.#",
            ".##.....#.",
            "",
            "Tile 3343:",
            "##..#...#.",
            "##.#..####",
            "........#.",
            "...#......",
            "......#...",
            "#.....#...",
            ".#.#.#....",
            "......##.#",
            "..#.##...#",
            ".#.#..####",
            "",
            "Tile 2017:",
            "##.##.#.##",
            "...#......",
            ".....##.#.",
            "..#...#...",
            "#.#....#..",
            "#....#...#",
            "#....#...#",
            "#...##....",
            ".....#.#.#",
            "....#.#.#.",
            "",
            "Tile 1151:",
            "##..##....",
            "..##.....#",
            "#.####.##.",
            "...##....#",
            "..##.....#",
            "....#....#",
            ".#.##...##",
            "......#..#",
            "#..#.#.#..",
            "#.###...##",
            "",
            "Tile 3697:",
            "###.....#.",
            "..##...##.",
            "......#.#.",
            ".#...#.#.#",
            ".#...#...#",
            ".#.....#..",
            "#...#.#...",
            "#..#....#.",
            ".#..##...#",
            "..#.#.##..",
            "",
            "Tile 1487:",
            "..#####.##",
            "#.........",
            "#.#...#.##",
            "##.##.....",
            ".##.#....#",
            "..#....###",
            "#.........",
            "..#.......",
            "#.##..#...",
            ".####..##.",
            "",
            "Tile 1481:",
            "..#..#..##",
            "#.#..#.##.",
            "#..##....#",
            "##.....#..",
            ".#...##.##",
            ".#......#.",
            "..#..##...",
            "###...###.",
            "..#...##..",
            "#...##.#..",
            "",
            "Tile 2903:",
            "#..#...###",
            "#..#####.#",
            "..#.......",
            "###..###..",
            ".#........",
            "#.#.#....#",
            "..#..#.###",
            "##..#.##..",
            "...#......",
            ".##.##.#..",
            "",
            "Tile 1171:",
            ".###..####",
            "....#..#..",
            "....#..#.#",
            "#.....##.#",
            "##...##...",
            "#..#.##.#.",
            "##..##...#",
            "#..##.#.#.",
            "#.#..##.#.",
            "##.##....#",
            "",
            "Tile 2621:",
            "#...###.##",
            ".#.....##.",
            ".#..#...#.",
            "...#..#.##",
            ".#..##..#.",
            "##..#..##.",
            "...##...#.",
            ".#..#...##",
            "....#....#",
            "...##.####",
            "",
            "Tile 2671:",
            "...#####..",
            "........##",
            "#....##..#",
            "##........",
            "##..##.#..",
            "..#.....#.",
            "#.........",
            "..##.....#",
            "##....#.##",
            "###.#...##",
            "",
            "Tile 2357:",
            ".##.##....",
            "#...#....#",
            "...##..#.#",
            "#.##...#..",
            "##.###..##",
            "..#..#####",
            "...#.###.#",
            "...##....#",
            "..#.####.#",
            "#..##..###",
            "",
            "Tile 3617:",
            "...##.#...",
            "#####..#.#",
            "....#....#",
            "......#..#",
            "#...#.....",
            "...##....#",
            "....#.....",
            "#..#...##.",
            "#...#...#.",
            "....##...#",
            "",
            "Tile 3517:",
            "#.#..#..#.",
            "#......#.#",
            ".#.#..#.##",
            "...##...#.",
            "#.##..##..",
            "...#..#.##",
            "#...#.##..",
            ".#..#..#.#",
            "....#...##",
            "..###.###.",
            "",
            "Tile 1499:",
            ".#..#....#",
            "...#.##..#",
            "#...#..#..",
            "..##...#..",
            "#....#.#.#",
            "#..#.#..##",
            "##.##..#.#",
            ".##.#..#.#",
            "#.###..##.",
            "##.##.#.#.",
            "",
            "Tile 3581:",
            "###.#.###.",
            ".#..#....#",
            "###....##.",
            "..#.#...##",
            "##..#..#..",
            ".....##...",
            "........#.",
            ".#.....##.",
            "#.#...##..",
            "###.#.#..#",
            "",
            "Tile 2687:",
            ".#####...#",
            ".......#..",
            ".#..#....#",
            "...#......",
            "##....#..#",
            "#.#..#..#.",
            "##..#.#.#.",
            "..#....###",
            ".#........",
            "#...#..#..",
            "",
            "Tile 1657:",
            "...#...#.#",
            ".#.##.#..#",
            "....#...#.",
            "...##.##..",
            "....#.#.##",
            "#..#..#.#.",
            "...#.#.#..",
            "...##....#",
            "#...#.#..#",
            ".##.##...#",
            "",
            "Tile 3449:",
            "#.....###.",
            "#..#.....#",
            "#......##.",
            "....#.....",
            "##....#...",
            ".....#...#",
            "..........",
            "#......###",
            ".#....#...",
            "###...#...",
            "",
            "Tile 3797:",
            "#.#######.",
            "..#.##.###",
            "...#.#...#",
            "##.##.##.#",
            "#.......#.",
            ".#......##",
            "##...#####",
            "..#.##.###",
            "#.#.##...#",
            "##..#.#.#.",
            "",
            "Tile 1511:",
            "#.###.###.",
            ".##.#....#",
            "........##",
            "##..#..#..",
            "..#####.##",
            ".....#....",
            ".......#..",
            "...#......",
            "##........",
            "...#.#####",
            "",
            "Tile 2311:",
            "..#.####..",
            "......#..#",
            ".###..#.#.",
            "##..#.###.",
            "...#...#..",
            ".......###",
            "##.#......",
            ".####...##",
            "##.##..###",
            "#...#..###",
            "",
            "Tile 1123:",
            ".##.##....",
            "#....##...",
            "#..#.....#",
            "#........#",
            "....#.....",
            "#...##.#.#",
            "#.#.##..##",
            "...#...##.",
            "#..#...#..",
            "##.#.#.###",
        };


    }
}

