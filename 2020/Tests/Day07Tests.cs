using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day07Tests
    {
        public static readonly string[] TEST_INPUT1 =
        {
            "light red bags contain 1 bright white bag, 2 muted yellow bags.",
            "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
            "bright white bags contain 1 shiny gold bag.",
            "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
            "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
            "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
            "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
            "faded blue bags contain no other bags.",
            "dotted black bags contain no other bags.",
        };

        public static readonly string[] TEST_INPUT2 =
        {
            "shiny gold bags contain 2 dark red bags.",
            "dark red bags contain 2 dark orange bags.",
            "dark orange bags contain 2 dark yellow bags.",
            "dark yellow bags contain 2 dark green bags.",
            "dark green bags contain 2 dark blue bags.",
            "dark blue bags contain 2 dark violet bags.",
            "dark violet bags contain no other bags.",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day07();
            Assert.AreEqual(
                4,
                day.Solve1(TEST_INPUT1, "shiny gold"));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day07();
            Assert.AreEqual(
                128,
                day.Solve1(Day07.PUZZLE_INPUT, "shiny gold"));
        }

        [TestMethod]
        public void Test2FromExample1()
        {
            var day = new Day07();
            Assert.AreEqual(
                32,
                day.Solve2(TEST_INPUT1, "shiny gold"));
        }

        [TestMethod]
        public void Test2FromExample2()
        {
            var day = new Day07();
            Assert.AreEqual(
                126,
                day.Solve2(TEST_INPUT2, "shiny gold"));
        }


        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day07();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day07.PUZZLE_INPUT, "shiny gold"));
        }

    }
}

