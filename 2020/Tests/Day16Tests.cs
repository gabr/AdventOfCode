using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day16Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "class: 1-3 or 5-7",
            "departure-row: 6-11 or 33-44",
            "departure-seat: 13-40 or 45-50",
            "",
            "your ticket:",
            "7,1,14",
            "",
            "nearby tickets:",
            "7,3,47",
            "40,4,50",
            "55,2,20",
            "38,6,12",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day16();
            Assert.AreEqual(
                71ul,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day16();
            Assert.AreEqual(
                22000ul,
                day.Solve1(Day16.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day16();
            Assert.AreEqual(
                7ul * 14ul,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day16();
            UInt64 solution = day.Solve2(Day16.PUZZLE_INPUT);
            Assert.AreNotEqual(562ul, solution);
            Assert.AreEqual(410460648673ul, solution);
        }

    }
}

