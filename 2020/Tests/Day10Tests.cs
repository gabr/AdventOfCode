using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day10Tests
    {
        public static readonly int[] TEST_INPUT1 =
        {
            16,
            10,
            15,
            5,
            1,
            11,
            7,
            19,
            6,
            12,
            4,
        };

        public static readonly int[] TEST_INPUT2 =
        {
            28,
            33,
            18,
            42,
            31,
            14,
            46,
            20,
            48,
            47,
            24,
            23,
            49,
            45,
            19,
            38,
            39,
            11,
            1,
            32,
            25,
            35,
            8,
            17,
            7,
            9,
            4,
            2,
            34,
            10,
            3,
        };

        [TestMethod]
        public void Test1FromExample1()
        {
            var day = new Day10();
            Assert.AreEqual(
                7*5,
                day.Solve1(TEST_INPUT1));
        }


        [TestMethod]
        public void Test1FromExample2()
        {
            var day = new Day10();
            Assert.AreEqual(
                22 * 10,
                day.Solve1(TEST_INPUT2));
        }

        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day10();
            Assert.AreEqual(
                2475,
                day.Solve1(Day10.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            //var day = new Day10();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(TEST_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day10();
            //Assert.AreEqual(
            //    ,
            //    day.Solve2(Day10.PUZZLE_INPUT));
        }

    }
}

