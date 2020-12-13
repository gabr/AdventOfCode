using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day13Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "939",
            "7,13,x,x,59,x,31,19,",
        };

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day13();
            Assert.AreEqual(
                295,
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day13();
            Assert.AreEqual(
                246,
                day.Solve1(Day13.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day13();
            Assert.AreEqual(
                1068781ul,
                day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2FromExample1()
        {
            var day = new Day13();
            Assert.AreEqual(1068781ul, day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2FromExample2()
        {
            var day = new Day13();
            Assert.AreEqual(3417ul, day.Solve2(new string[] { "", "17,x,13,19,", }));
        }

        [TestMethod]
        public void Test2FromExample3()
        {
            var day = new Day13();
            Assert.AreEqual(754018ul, day.Solve2(new string[] { "", "67,7,59,61,", }));
        }

        [TestMethod]
        public void Test2FromExample4()
        {
            var day = new Day13();
            Assert.AreEqual(779210ul, day.Solve2(new string[] { "", "67,x,7,59,61,", }));
        }

        [TestMethod]
        public void Test2FromExample5()
        {
            var day = new Day13();
            Assert.AreEqual(1261476ul, day.Solve2(new string[] { "", "67,7,x,59,61,", }));
        }

        [TestMethod]
        public void Test2FromExample6()
        {
            var day = new Day13();
            Assert.AreEqual(1202161486ul, day.Solve2(new string[] { "", "1789,37,47,1889,", }));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day13();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day13.PUZZLE_INPUT));
        }

    }
}

