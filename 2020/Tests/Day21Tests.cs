using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day21Tests
    {
        public static readonly string[] TEST_INPUT =
        {
            "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
            "trh fvjkl sbzzf mxmxvkd (contains dairy)",
            "sqjhc fvjkl (contains soy)",
            "sqjhc mxmxvkd sbzzf (contains fish)",
        };

        [TestMethod]
        public void Test1FromExamples()
        {
            var day = new Day21();
            Assert.AreEqual(
                5, // kfcds, nhms, sbzzf x2, trh
                day.Solve1(TEST_INPUT));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day21();
            Assert.AreEqual(
                1685,
                day.Solve1(Day21.PUZZLE_INPUT));
        }

        //[TestMethod]
        //public void Test2FromExample()
        //{
        //    var day = new Day21();
        //    Assert.AreEqual(
        //        ,
        //        day.Solve2(TEST_INPUT));
        //}

        //[TestMethod]
        //public void Test2PuzzleInput()
        //{
        //    var day = new Day21();
        //    Assert.AreEqual(
        //        ,
        //        day.Solve2(Day21.PUZZLE_INPUT));
        //}

    }
}

