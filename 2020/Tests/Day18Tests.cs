using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day18Tests
    {
        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day18();

            Assert.AreEqual(71ul,    day.Solve1(new [] {"1 + 2 * 3 + 4 * 5 + 6"}));
            Assert.AreEqual(51ul,    day.Solve1(new [] {"1 + (2 * 3) + (4 * (5 + 6))"}));
            Assert.AreEqual(26ul,    day.Solve1(new [] {"2 * 3 + (4 * 5)"}));
            Assert.AreEqual(437ul,   day.Solve1(new [] {"5 + (8 * 3 + 9 + 3 * 4 * 3)"}));
            Assert.AreEqual(12240ul, day.Solve1(new [] {"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"}));
            Assert.AreEqual(13632ul, day.Solve1(new [] {"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"}));
            Assert.AreEqual(17ul,    day.Solve1(new [] {"2 + (3 * (1 + 4 ) )"}));
            Assert.AreEqual(5ul,     day.Solve1(new [] {"((2) + (3))"}));
            Assert.AreEqual(8ul,     day.Solve1(new [] {"((2) + (3 * 2))"}));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day18();
            Assert.AreEqual(
                11004703763391ul,
                day.Solve1(Day18.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day18();

            Assert.AreEqual(51ul,     day.Solve2(new [] { "1 + (2 * 3) + (4 * (5 + 6))" }));
            Assert.AreEqual(46ul,     day.Solve2(new [] { "2 * 3 + (4 * 5)" }));
            Assert.AreEqual(1445ul,   day.Solve2(new [] { "5 + (8 * 3 + 9 + 3 * 4 * 3)" }));
            Assert.AreEqual(669060ul, day.Solve2(new [] { "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" }));
            Assert.AreEqual(23340ul,  day.Solve2(new [] { "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" }));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day18();
            Assert.AreEqual(
                290726428573651ul,
                day.Solve2(Day18.PUZZLE_INPUT));
        }

    }
}

