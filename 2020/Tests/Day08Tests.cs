using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day010Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "nop +0",
            "acc +1",
            "jmp +4",
            "acc +3",
            "jmp -3",
            "acc -99",
            "acc +1",
            "jmp -4",
            "acc +6",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day08();
            Assert.AreEqual(
                5,
                day.Solve1(TEST_INPUT));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day08();
            Assert.AreEqual(
                1179,
                day.Solve1(Day08.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day08();
            Assert.AreEqual(
                8,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day08();
            Assert.AreEqual(
                1089,
                day.Solve2(Day08.PUZZLE_INPUT));
        }

    }
}

