using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Solutions
{
    [TestClass]
    public class Day09Tests
    {

        [TestMethod]
        public void Test1FromExample()
        {
            var day = new Day09();
            Assert.AreEqual(32,     day.Solve(9,  25));
            Assert.AreEqual(8317,   day.Solve(10, 1618));
            Assert.AreEqual(146373, day.Solve(13, 7999));
            Assert.AreEqual(2764,   day.Solve(17, 1104));
            Assert.AreEqual(54718,  day.Solve(21, 6111));
            Assert.AreEqual(37305,  day.Solve(30, 5807));
        }

    }
}

