using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day14Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
            "mem[8] = 11",
            "mem[7] = 101",
            "mem[8] = 0",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day14();
            Assert.AreEqual(
                165ul,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day14();
            // 52056536420 <-- too low
            Assert.IsTrue(52056536420 < day.Solve1(Day14.PUZZLE_INPUT));

            //Assert.AreEqual(
            //    ,
            //    day.Solve1(Day14.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day14();
            //Assert.AreEqual(
            //    1068781ul,
            //    day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day14();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day14.PUZZLE_INPUT));
        }

    }
}

