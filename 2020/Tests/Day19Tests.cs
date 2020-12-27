using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day19Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "0: 4 1 5",
            "1: 2 3 | 3 2",
            "2: 4 4 | 5 5",
            "3: 4 5 | 5 4",
            "4: \"a\"",
            "5: \"b\"",
            "",
            "ababbb",
            "bababa",
            "abbbab",
            "aaabbb",
            "aaaabbb",
        };

        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day19();
            Assert.AreEqual(
                2,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day19();
            Assert.AreEqual(
                160,
                day.Solve1(Day19.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day19();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day19();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day19.PUZZLE_INPUT));
        }

    }
}

