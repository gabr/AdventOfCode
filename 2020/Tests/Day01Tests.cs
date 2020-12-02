using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day01Tests
    {
        [TestMethod]
        public void Test1FromExample()
        {
            var day01 = new Day01();

            Assert.AreEqual(
                514579,
                day01.Solve1(
                    new long[]
                    {
                        1721,
                        979,
                        366,
                        299,
                        675,
                        1456,
                    }));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day01 = new Day01();
            Assert.AreEqual(
                956091,
                day01.Solve1(Day01.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day01 = new Day01();

            Assert.AreEqual(
                241861950,
                day01.Solve2(
                    new long[]
                    {
                        1721,
                        979,
                        366,
                        299,
                        675,
                        1456,
                    }));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day01 = new Day01();
            Assert.AreEqual(
                79734368,
                day01.Solve2(Day01.PUZZLE_INPUT));
        }


    }
}

