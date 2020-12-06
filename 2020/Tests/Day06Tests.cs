using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day06Tests
    {
        private static readonly string[] TEST_INPUT =
        {
            "abc",
            "",
            "a",
            "b",
            "c",
            "",
            "ab",
            "ac",
            "",
            "a",
            "a",
            "a",
            "a",
            "",
            "b",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day06();
            Assert.AreEqual(
                11,
                day.Solve1(TEST_INPUT));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day06();
            Assert.AreEqual(
                7027,
                day.Solve1(Day06.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day06();
            Assert.AreEqual(
                6,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day06();
            Assert.AreEqual(
                3579,
                day.Solve2(Day06.PUZZLE_INPUT));
        }

    }
}

