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
            "row: 6-11 or 33-44",
            "seat: 13-40 or 45-50",
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
                day.Solve(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            //var day = new Day16();
            //Assert.AreEqual(
            //    ,
            //    day.Solve());
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day16();
            //Assert.AreEqual(
            //    ,
            //    day.Solve());
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day16();
            //Assert.AreEqual(
            //    ,
            //    day.Solve(Day16.PUZZLE_INPUT, 30000000ul));
        }

    }
}

