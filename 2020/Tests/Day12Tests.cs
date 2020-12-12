using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day12Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "F10",
            "N3",
            "F7",
            "R90",
            "F11",
        };

        [TestMethod]
        public void Test1FromExample1()
        {
            var day = new Day12();
            Assert.AreEqual(
                25,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day12();
            Assert.AreEqual(
                1565,
                day.Solve1(Day12.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample1()
        {
            var day = new Day12();
            Assert.AreEqual(
                286,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day12();
            Assert.AreEqual(
                78883,
                day.Solve2(Day12.PUZZLE_INPUT));
        }

    }
}

