using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day14Tests
    {
        public static readonly string[] TEST_INPUT_1 =
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
                day.Solve1(TEST_INPUT_1));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day14();
            Assert.AreEqual(
                7817357407588ul,
                day.Solve1(Day14.PUZZLE_INPUT));
        }


        public static readonly string[] TEST_INPUT_2 =
        {
            "mask = 000000000000000000000000000000X1001X",
            "mem[42] = 100",
            "mask = 00000000000000000000000000000000X0XX",
            "mem[26] = 1",
        };

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day14();
            Assert.AreEqual(
                208ul,
                day.Solve2(TEST_INPUT_2));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day14();
            Assert.AreEqual(
                4335927555692ul,
                day.Solve2(Day14.PUZZLE_INPUT));
        }

    }
}

