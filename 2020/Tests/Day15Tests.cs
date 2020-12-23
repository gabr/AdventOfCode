using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day15Tests
    {
        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day15();

            Assert.AreEqual(436ul,  day.Solve(new int[] {0,3,6}, 2020ul));
            Assert.AreEqual(1ul,    day.Solve(new int[] {1,3,2}, 2020ul));
            Assert.AreEqual(10ul,   day.Solve(new int[] {2,1,3}, 2020ul));
            Assert.AreEqual(27ul,   day.Solve(new int[] {1,2,3}, 2020ul));
            Assert.AreEqual(78ul,   day.Solve(new int[] {2,3,1}, 2020ul));
            Assert.AreEqual(438ul,  day.Solve(new int[] {3,2,1}, 2020ul));
            Assert.AreEqual(1836ul, day.Solve(new int[] {3,1,2}, 2020ul));
        }


        [TestMethod]
        public void Test1PuzzleInput()
        {
            var day = new Day15();
            Assert.AreEqual(
                1294ul,
                day.Solve(Day15.PUZZLE_INPUT, 2020ul));
        }

        [TestMethod]
        public void Test2FromExample()
        {
            var day = new Day15();

            Assert.AreEqual(175594ul,  day.Solve(new int[] {0,3,6}, 30000000ul));
            Assert.AreEqual(2578ul,    day.Solve(new int[] {1,3,2}, 30000000ul));
            Assert.AreEqual(3544142ul, day.Solve(new int[] {2,1,3}, 30000000ul));
            Assert.AreEqual(261214ul,  day.Solve(new int[] {1,2,3}, 30000000ul));
            Assert.AreEqual(6895259ul, day.Solve(new int[] {2,3,1}, 30000000ul));
            Assert.AreEqual(18ul,      day.Solve(new int[] {3,2,1}, 30000000ul));
            Assert.AreEqual(362ul,     day.Solve(new int[] {3,1,2}, 30000000ul));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            //var day = new Day15();
            //Assert.AreEqual(
            //    ,
            //    day.Solve(Day15.PUZZLE_INPUT, 30000000ul));
        }

    }
}

