using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day05Tests
    {
        private static readonly string[] TEST_INPUT =
        {
            "FBFBBFFRLR",
            "BFFFBBFRRR",
            "FFFBBBFRRR",
            "BBFFBBFRLL",
        };

        [TestMethod]
        public void CalculatesCorrectSeatId()
        {
            var day = new Day05();

            Assert.AreEqual(357, day.CalculateSeatId("FBFBBFFRLR"));
            Assert.AreEqual(567, day.CalculateSeatId("BFFFBBFRRR"));
            Assert.AreEqual(119, day.CalculateSeatId("FFFBBBFRRR"));
            Assert.AreEqual(820, day.CalculateSeatId("BBFFBBFRLL"));
        }

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day05();
            Assert.AreEqual(
                820,
                day.Solve1(TEST_INPUT));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day05();
            Assert.AreEqual(
                933,
                day.Solve1(Day05.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day05();
            //Assert.AreEqual(
            //    2,
            //    day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day05();
            Assert.AreEqual(
                711,
                day.Solve2(Day05.PUZZLE_INPUT));
        }

    }
}

