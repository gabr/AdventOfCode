using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day20Tests
    {

    /*

        tile: 2311: [[210,  89, 231, 498], [300, 498, 924,  89], [924, 318, 300, 616], [231, 616, 210, 318]]
        tile: 1951: [[710, 498, 564, 841], [397, 841, 177, 498], [177, 587, 397, 318], [564, 318, 710, 587]]
        tile: 1171: [[966, 288,  24, 902], [399, 902,  96, 288], [ 96, 391, 399,  18], [ 24,  18, 966, 391]]
        tile: 1427: [[948, 234, 210, 576], [183, 576, 300, 234], [300,   9, 183, 348], [210, 348, 948,   9]]
        tile: 1489: [[848,  18, 948, 565], [ 43, 565, 183,  18], [183, 689,  43, 288], [948, 288, 848, 689]]
        tile: 2473: [[542, 116, 234, 966], [481, 966, 348, 116], [348, 399, 481, 184], [234, 184, 542, 399]]
        tile: 2971: [[161, 565,  85, 456], [532, 456, 680, 565], [680,  78, 532, 689], [ 85, 689, 161,  78]]
        tile: 2729: [[ 85, 576, 710, 271], [680, 271, 397, 576], [397, 962, 680,   9], [710,   9,  85, 962]]
        tile: 3079: [[702, 264, 184, 616], [501, 616, 116, 264], [116,  89, 501,  66], [184,  66, 702,  89]]

        #...##.#.. ..###..### #.#.#####.
        ..#.#..#.# ###...#.#. .#..######
        .###....#. ..#....#.. ..#.......
        ###.##.##. .#.#.#..## ######....
        .###.##### ##...#.### ####.#..#.
        .##.#....# ##.##.###. .#...#.##.
        #...###### ####.#...# #.#####.##
        .....#..## #...##..#. ..#.###...
        #.####...# ##..#..... ..#.......
        #.##...##. ..##.#..#. ..#.###...

        #.##...##. ..##.#..#. ..#.###...
        ##..#.##.. ..#..###.# ##.##....#
        ##.####... .#.####.#. ..#.###..#
        ####.#.#.. ...#.##### ###.#..###
        .#.####... ...##..##. .######.##
        .##..##.#. ....#...## #.#.#.#...
        ....#..#.# #.#.#.##.# #.###.###.
        ..#.#..... .#.##.#..# #.###.##..
        ####.#.... .#..#.##.. .######...
        ...#.#.#.# ###.##.#.. .##...####

        ...#.#.#.# ###.##.#.. .##...####
        ..#.#.###. ..##.##.## #..#.##..#
        ..####.### ##.#...##. .#.#..#.##
        #..#.#..#. ...#.#.#.. .####.###.
        .#..####.# #..#.#.#.# ####.###..
        .#####..## #####...#. .##....##.
        ##.##..#.. ..#...#... .####...#.
        #.#.###... .##..##... .####.##.#
        #...###... ..##...#.. ...#..####
        ..#.#....# ##.#.#.... ...##.....

        [564, 318, 710, 587]
        [231, 616, 210, 318]
        [702, 264, 184, 616]

        [710, 9, 85, 962]
        [210, 348, 948, 9]
        [184, 481, 399, 348]

        [85, 689, 161, 78]
        [948, 288, 848, 689]
        [399, 902, 96, 288]

        1951 3,0   2311 3,0   3079 0,0
        2729 3,0   1427 3,0   2473 3,1
        2971 3,0   1489 3,0   1171 1,0

        tile [0][0]: 1951  fits: trasformation: 3, rotation: 0
         tile [1][0]: 2311  fits: trasformation: 3, rotation: 0
          tile [2][0]: 3079  fits: trasformation: 0, rotation: 0
           tile [0][1]: 2729  fits: trasformation: 3, rotation: 0
            tile [1][1]: 1427  fits: trasformation: 3, rotation: 0

     */

        public static readonly string[] TEST_INPUT =
        {
            "Tile 2311:",
            "..##.#..#.",
            "##..#.....",
            "#...##..#.",
            "####.#...#",
            "##.##.###.",
            "##...#.###",
            ".#.#.#..##",
            "..#....#..",
            "###...#.#.",
            "..###..###",
            "",
            "Tile 1951:",
            "#.##...##.",
            "#.####...#",
            ".....#..##",
            "#...######",
            ".##.#....#",
            ".###.#####",
            "###.##.##.",
            ".###....#.",
            "..#.#..#.#",
            "#...##.#..",
            "",
            "Tile 1171:",
            "####...##.",
            "#..##.#..#",
            "##.#..#.#.",
            ".###.####.",
            "..###.####",
            ".##....##.",
            ".#...####.",
            "#.##.####.",
            "####..#...",
            ".....##...",
            "",
            "Tile 1427:",
            "###.##.#..",
            ".#..#.##..",
            ".#.##.#..#",
            "#.#.#.##.#",
            "....#...##",
            "...##..##.",
            "...#.#####",
            ".#.####.#.",
            "..#..###.#",
            "..##.#..#.",
            "",
            "Tile 1489:",
            "##.#.#....",
            "..##...#..",
            ".##..##...",
            "..#...#...",
            "#####...#.",
            "#..#.#.#.#",
            "...#.#.#..",
            "##.#...##.",
            "..##.##.##",
            "###.##.#..",
            "",
            "Tile 2473:",
            "#....####.",
            "#..#.##...",
            "#.##..#...",
            "######.#.#",
            ".#...#.#.#",
            ".#########",
            ".###.#..#.",
            "########.#",
            "##...##.#.",
            "..###.#.#.",
            "",
            "Tile 2971:",
            "..#.#....#",
            "#...###...",
            "#.#.###...",
            "##.##..#..",
            ".#####..##",
            ".#..####.#",
            "#..#.#..#.",
            "..####.###",
            "..#.#.###.",
            "...#.#.#.#",
            "",
            "Tile 2729:",
            "...#.#.#.#",
            "####.#....",
            "..#.#.....",
            "....#..#.#",
            ".##..##.#.",
            ".#.####...",
            "####.#.#..",
            "##.####...",
            "##..#.##..",
            "#.##...##.",
            "",
            "Tile 3079:",
            "#.#.#####.",
            ".#..######",
            "..#.......",
            "######....",
            "####.#..#.",
            ".#...#.##.",
            "#.#####.##",
            "..#.###...",
            "..#.......",
            "..#.###...",
        };

        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day20();
            Assert.AreEqual(
                20899048083289ul,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day20();
            Assert.AreEqual(
                20033377297069ul,
                day.Solve1(Day20.PUZZLE_INPUT));
        }

        [TestMethod]
        public void TestTile()
        {
            var tileLines = new []
            {
                "Tile 3079:",
                "#.#.#####.",
                ".#..######",
                "..#.......",
                "######....",
                "####.#..#.",
                ".#...#.##.",
                "#.#####.##",
                "..#.###...",
                "..#.......",
                "..#.###...",
            };

            var possibleEdges = new Int32[][]
            {
                new Int32[] { 0b_10101_11110, 0b_01000_01000, 0b_00101_11000, 0b_10011_01000, },
                new Int32[] { 0b_01111_10101, 0b_10011_01000, 0b_00011_10100, 0b_01000_01000, },
                new Int32[] { 0b_00011_10100, 0b_00010_00010, 0b_01111_10101, 0b_00010_11001, },
                new Int32[] { 0b_00101_11000, 0b_00010_00010, 0b_10101_11110, 0b_00010_11001, },
            };

            var tile = new Day20.Tile(tileLines);

            Assert.AreEqual(tileLines.Length - 1, tile.Lines.Length);
            for (int i = 1; i < tileLines.Length; i++)
                Assert.AreEqual(tileLines[i], tile.Lines[i-1]);

            Assert.AreEqual(4, tile.PossibleEdges.Length);
            for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                Assert.AreEqual(possibleEdges[i][j], tile.PossibleEdges[i][j], $"{i}, {j}");

            for (int i = 0; i < 4; i++)
            {
                Assert.AreEqual(possibleEdges[i][0], tile.TopEdge);
                Assert.AreEqual(possibleEdges[i][1], tile.RightEdge);
                Assert.AreEqual(possibleEdges[i][2], tile.BottomEdge);
                Assert.AreEqual(possibleEdges[i][3], tile.LeftEdge);

                tile.SwitchToNextTransformation();
                Assert.AreEqual(possibleEdges[i][1], tile.TopEdge);
                Assert.AreEqual(possibleEdges[i][2], tile.RightEdge);
                Assert.AreEqual(possibleEdges[i][3], tile.BottomEdge);
                Assert.AreEqual(possibleEdges[i][0], tile.LeftEdge);

                tile.SwitchToNextTransformation();
                Assert.AreEqual(possibleEdges[i][2], tile.TopEdge);
                Assert.AreEqual(possibleEdges[i][3], tile.RightEdge);
                Assert.AreEqual(possibleEdges[i][0], tile.BottomEdge);
                Assert.AreEqual(possibleEdges[i][1], tile.LeftEdge);

                tile.SwitchToNextTransformation();
                Assert.AreEqual(possibleEdges[i][3], tile.TopEdge);
                Assert.AreEqual(possibleEdges[i][0], tile.RightEdge);
                Assert.AreEqual(possibleEdges[i][1], tile.BottomEdge);
                Assert.AreEqual(possibleEdges[i][2], tile.LeftEdge);

                tile.SwitchToNextTransformation();
            }

            Assert.AreEqual(possibleEdges[0][0], tile.TopEdge);
            Assert.AreEqual(possibleEdges[0][1], tile.RightEdge);
            Assert.AreEqual(possibleEdges[0][2], tile.BottomEdge);
            Assert.AreEqual(possibleEdges[0][3], tile.LeftEdge);

            for (int i = 0; i < 123; i++)
                tile.SwitchToNextTransformation();

            tile.ResetTrasfrmation();
            Assert.AreEqual(possibleEdges[0][0], tile.TopEdge);
            Assert.AreEqual(possibleEdges[0][1], tile.RightEdge);
            Assert.AreEqual(possibleEdges[0][2], tile.BottomEdge);
            Assert.AreEqual(possibleEdges[0][3], tile.LeftEdge);
        }

        [TestMethod]
        public void TestAssemblesTiles()
        {
            var tiles = Day20.Tile.ParseTiles(TEST_INPUT);
            Day20.AssembleTilesIntoImage(tiles);
        }

        /*
        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day20();
            Assert.AreEqual(
                ,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day20();
            Assert.AreEqual(
                ,
                day.Solve2(Day20.PUZZLE_INPUT));
        }
        */

    }
}

