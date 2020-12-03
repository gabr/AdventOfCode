using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day03Tests
    {
        private static readonly string[] TEST_MAP = new string[]
        {
            "..##.......",
            "#...#...#..",
            ".#....#..#.",
            "..#.#...#.#",
            ".#...##..#.",
            "..#.##.....",
            ".#.#.#....#",
            ".#........#",
            "#.##...#...",
            "#...##....#",
            ".#..#...#.#",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day03();
            Assert.AreEqual(
                7,
                day.Solve1(TEST_MAP));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day03();
            Assert.AreEqual(
                173,
                day.Solve1(Day03.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day03();
            Assert.AreEqual(
                336,
                day.Solve2(TEST_MAP));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day03();
            Assert.AreEqual(
                4385176320,
                day.Solve2(Day03.PUZZLE_INPUT));
        }

    }
}

