using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day23Tests
    {
        public static readonly string TEST_INPUT = "389125467";

        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day23();
            Assert.AreEqual("92658374", day.Solve1(TEST_INPUT, 10));
            Assert.AreEqual("67384529", day.Solve1(TEST_INPUT, 100));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day23();
            Assert.AreEqual(
                "82573496",
                day.Solve1(Day23.PUZZLE_INPUT, 100));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day23();
            Assert.AreEqual(
                149245887792ul,
                day.Solve2(TEST_INPUT, 10000000ul));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day23();
            Assert.AreEqual(
                11498506800ul,
                day.Solve2(Day23.PUZZLE_INPUT, 10000000ul));
        }

    }
}

