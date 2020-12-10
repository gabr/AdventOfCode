using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day09Tests
    {
        public static readonly UInt64[] TEST_INPUT =
        {
            35,
            20,
            15,
            25,
            47,
            40,
            62,
            55,
            65,
            95,
            102,
            117,
            150,
            182,
            127,
            219,
            299,
            277,
            309,
            576,
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day09();
            Assert.AreEqual(
                127ul,
                day.Solve1(TEST_INPUT, 5));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day09();
            Assert.AreEqual(
                14360655ul,
                day.Solve1(Day09.PUZZLE_INPUT, 25));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day09();
            Assert.AreEqual(
                62ul,
                day.Solve2(TEST_INPUT, 5));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day09();
            Assert.AreEqual(
                1962331ul,
                day.Solve2(Day09.PUZZLE_INPUT, 25));
        }

    }
}

