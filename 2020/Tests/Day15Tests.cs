using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day15Tests
    {
        public static readonly string[] TEST_INPUT =
        {
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day15();
            Assert.AreEqual(
                1,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            //Assert.AreEqual(
            //    ,
            //    day.Solve1(Day15.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day15();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day15();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day15.PUZZLE_INPUT));
        }

    }
}

