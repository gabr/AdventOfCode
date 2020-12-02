using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day02Tests
    {
        [TestMethod]
        public void Test1FromExample()
        {
            var day02 = new Day02();
            Assert.AreEqual(
                2,
                day02.Solve1(
                    new string[]
                    {
                        "1-3 a: abcde",
                        "1-3 b: cdefg",
                        "2-9 c: ccccccccc",
                    }));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day02 = new Day02();
            Assert.AreEqual(
                439,
                day02.Solve1(Day02.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day02 = new Day02();
            Assert.AreEqual(
                1,
                day02.Solve2(
                    new string[]
                    {
                        "1-3 a: abcde",
                        "1-3 b: cdefg",
                        "2-9 c: ccccccccc",
                    }));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day02 = new Day02();
            Assert.AreEqual(
                584,
                day02.Solve2(Day02.PUZZLE_INPUT));
        }

    }
}

