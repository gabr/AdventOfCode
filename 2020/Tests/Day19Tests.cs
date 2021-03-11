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
            var day = new Day19();
            Assert.AreEqual(
                12,
                day.Solve2(new string[]
                {
                    "42: 9 14 | 10 1",
                    "9: 14 27 | 1 26",
                    "10: 23 14 | 28 1",
                    "1: \"a\"",
                    "11: 42 31",
                    "5: 1 14 | 15 1",
                    "19: 14 1 | 14 14",
                    "12: 24 14 | 19 1",
                    "16: 15 1 | 14 14",
                    "31: 14 17 | 1 13",
                    "6: 14 14 | 1 14",
                    "2: 1 24 | 14 4",
                    "0: 8 11",
                    "13: 14 3 | 1 12",
                    "15: 1 | 14",
                    "17: 14 2 | 1 7",
                    "23: 25 1 | 22 14",
                    "28: 16 1",
                    "4: 1 1",
                    "20: 14 14 | 1 15",
                    "3: 5 14 | 16 1",
                    "27: 1 6 | 14 18",
                    "14: \"b\"",
                    "21: 14 1 | 1 14",
                    "25: 1 1 | 1 14",
                    "22: 14 14",
                    "8: 42",
                    "26: 14 22 | 1 20",
                    "18: 15 15",
                    "7: 14 5 | 1 21",
                    "24: 14 1",
                     "",
                    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
                    "bbabbbbaabaabba",
                    "babbbbaabbbbbabbbbbbaabaaabaaa",
                    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
                    "bbbbbbbaaaabbbbaaabbabaaa",
                    "bbbababbbbaaaaaaaabbababaaababaabab",
                    "ababaaaaaabaaab",
                    "ababaaaaabbbaba",
                    "baabbaaaabbaaaababbaababb",
                    "abbbbabbbbaaaababbbbbbaaaababb",
                    "aaaaabbaabaaaaababaa",
                    "aaaabbaaaabbaaa",
                    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
                    "babaaabbbaaabaababbaabababaaab",
                    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",
            }));
        }

        [TestMethod]
        public void CustomTests()
        {
            var rules1 = new string[]
            {
                "0: 1 3 2",
                "1: \"a\"",
                "2: \"b\"",
                "3: 1 | 1 3",
                "",
            };

            var rules1tasks = new (string line, bool expectedAnswer)[]
            {
                ("",      false),
                ("a",     false),
                ("aa",    false),
                ("aab",   true),
                ("aaab",  true),
                ("aaaab", true),
                ("aaaaa", false),
            };

            var rules2 = new string[]
            {
                "0: 1 3 2",
                "1: \"a\"",
                "2: \"b\"",
                "3: 1 2 | 1 3 2",
                ""
            };

            var rules2tasks = new (string line, bool expectedAnswer)[]
            {
                ("ab",      false),
                ("aabb",    true),
                ("aaabbb",  true),
                ("aaabbbb", false),
                ("aaaabbb", false),
            };

            var rules3 = new string[]
            {
                "0: 8 11",
                "1: \"a\"",
                "2: 1 24 | 14 4",
                "3: 5 14 | 16 1",
                "4: 1 1",
                "5: 1 14 | 15 1",
                "6: 14 14 | 1 14",
                "7: 14 5 | 1 21",
                "8: 42",
                "9: 14 27 | 1 26",
                "10: 23 14 | 28 1",
                "11: 42 31",
                "12: 24 14 | 19 1",
                "13: 14 3 | 1 12",
                "14: \"b\"",
                "15: 1 | 14",
                "16: 15 1 | 14 14",
                "17: 14 2 | 1 7",
                "18: 15 15",
                "19: 14 1 | 14 14",
                "20: 14 14 | 1 15",
                "21: 14 1 | 1 14",
                "22: 14 14",
                "23: 25 1 | 22 14",
                "24: 14 1",
                "25: 1 1 | 1 14",
                "26: 14 22 | 1 20",
                "27: 1 6 | 14 18",
                "28: 16 1",
                "31: 14 17 | 1 13",
                "42: 9 14 | 10 1",
                 "",
            };


            var rules3tasks = new (string line, bool expectedAnswer)[]
            {
                ("abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa", false),
                ("bbabbbbaabaabba",                               true),
                ("babbbbaabbbbbabbbbbbaabaaabaaa",                true),
                ("aaabbbbbbaaaabaababaabababbabaaabbababababaaa", true),
                ("bbbbbbbaaaabbbbaaabbabaaa",                     true),
                ("bbbababbbbaaaaaaaabbababaaababaabab",           true),
                ("ababaaaaaabaaab",                               true),
                ("ababaaaaabbbaba",                               true),
                ("baabbaaaabbaaaababbaababb",                     true),
                ("abbbbabbbbaaaababbbbbbaaaababb",                true),
                ("aaaaabbaabaaaaababaa",                          true),
                ("aaaabbaaaabbaaa",                               false),
                ("aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",           true),
                ("babaaabbbaaabaababbaabababaaab",                false),
                ("aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",      true),
            };

            var day = new Day19();

            void test(string[] rulesStrings, (string line, bool expectedAnswer)[] tasks)
            {
                int messagesStartIndex = 0;
                var rules = new Day19.Rule[1024];

                day.ParseReceivedMessages(rulesStrings, out messagesStartIndex, in rules);

                // Change rules
                // 8: 42 | 42 8
                // 11: 42 31 | 42 11 31
                rules[8]  = Day19.Rule.FromString("8: 42 | 42 8");
                rules[11] = Day19.Rule.FromString("11: 42 31 | 42 11 31");

                foreach (var task in tasks)
                {
                    bool isValid = day.IsValidMessage(task.line, rules);
                    Assert.AreEqual(task.expectedAnswer, isValid, task.line);
                }
            }

            test(rules1, rules1tasks);
            test(rules2, rules2tasks);
            test(rules3, rules3tasks);
        }

        [TestMethod]
        public void Test2PuzzleInputWrongResults()
        {
            var day = new Day19();
            Assert.AreNotEqual(160, day.Solve2(Day19.PUZZLE_INPUT));
            Assert.AreNotEqual(211, day.Solve2(Day19.PUZZLE_INPUT));
        }

        [TestMethod]
        public void Test2PuzzleInput()
        {
            var day = new Day19();
            Assert.AreEqual(
                357,
                day.Solve2(Day19.PUZZLE_INPUT));
        }

    }
}

