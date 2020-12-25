using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day17Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            ".#.",
            "..#",
            "###",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day17();
            Assert.AreEqual(
                112,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day17();
            Assert.AreEqual(
                276,
                day.Solve1(Day17.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day17();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day17();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day17.PUZZLE_INPUT));
        }

    }
}

