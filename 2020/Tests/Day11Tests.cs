
using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day11Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "L.LL.LL.LL",
            "LLLLLLL.LL",
            "L.L.L..L..",
            "LLLL.LL.LL",
            "L.LL.LL.LL",
            "L.LLLLL.LL",
            "..L.L.....",
            "LLLLLLLLLL",
            "L.LLLLLL.L",
            "L.LLLLL.LL",
        };

        [TestMethod]
        public void Test1FromExample1()
        {
            var day = new Day11();
            Assert.AreEqual(
                37,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day11();
            Assert.AreEqual(
                2277,
                day.Solve1(Day11.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample1()
        {
            var day = new Day11();
            Assert.AreEqual(
                26,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day11();
            Assert.AreEqual(
                2066,
                day.Solve2(Day11.PUZZLE_INPUT));
        }

    }
}

