using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day22Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "Player 1:",
            "9",
            "2",
            "6",
            "3",
            "1",
            "",
            "Player 2:",
            "5",
            "8",
            "4",
            "7",
            "10",
        };

        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day22();
            Assert.AreEqual(
                306,
                day.Solve1(TEST_INPUT));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day22();
            Assert.AreEqual(
                33559,
                day.Solve1(Day22.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day22();
            Assert.AreEqual(
                291,
                day.Solve2(TEST_INPUT));
        }

        /*
        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day22();
            Assert.AreEqual(
                ,
                day.Solve2(Day22.PUZZLE_INPUT));
        }
        */

    }
}

