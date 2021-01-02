using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day25Tests
    {
        public static readonly UInt64[] TEST_INPUT =
        {
            5764801ul,
            17807724ul
        };

        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day25();

            Assert.AreEqual( 8, day.DetermineLoopSize(TEST_INPUT[0]));
            Assert.AreEqual(11, day.DetermineLoopSize(TEST_INPUT[1]));

            Assert.AreEqual(14897079ul, day.Solve1(TEST_INPUT));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day25();
            Assert.AreEqual(
                11288669ul,
                day.Solve1(Day25.PUZZLE_INPUT));
        }

        //[TestMethod]
        //public void Test2FromExample()
        //{
        //    var day = new Day25();
        //    Assert.AreEqual(
        //        ,
        //        day.Solve2(TEST_INPUT));
        //}

        //[TestMethod]
        //public void Test2PuzzleInput()
        //{
        //    var day = new Day25();
        //    Assert.AreEqual(
        //        ,
        //        day.Solve2(Day25.PUZZLE_INPUT));
        //}

    }
}

