using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day19
    {
        public class Rule
        {
            public int Id { get; private set; }

            public Rule(int id)
            {
                Id = id;
            }

            public static Rule FromString(ReadOnlySpan<char> line)
            {
                Rule rule = null;

                int idLength = line.IndexOf(':');
                int id = int.Parse(line.Slice(0, idLength));

                line = line.Slice(idLength + 2);
                bool isCharRule = line[0] == '"';

                if (isCharRule)
                    rule = new CharRule(id, line);
                else
                    rule = new IdsRule(id, line);

                return rule;
            }
        }

        public class IdsRule : Rule
        {
            public int[][] Ids { get; private set; }

            public IdsRule(int id, ReadOnlySpan<char> line) : base(id)
            {
                Ids = line
                    .ToString()
                    .Split('|')
                    .Select(s => s
                        .Trim()
                        .Split(' ')
                        .Select(v => int.Parse(v))
                        .ToArray())
                    .ToArray();
            }

            public override string ToString() => $"{Id}: {string.Join(" | ", Ids.Select(i => string.Join(" ", i)))}";
        }

        public class CharRule : Rule
        {
            public char Char { get; private set; }

            public CharRule(int id, ReadOnlySpan<char> line) : base(id)
            {
                Char = line[1];
            }

            public override string ToString() => $"{Id}: \"{Char}\"";
        }

        public class Step
        {
            public IdsRule Rule;
            public int     IdsSetIndex;
            public int     IdIndex;

            public Step(IdsRule rule)
            {
                Rule        = rule;
                IdsSetIndex = 0;
                IdIndex     = -1;
            }

            public Step(Step step)
            {
                Rule        = step.Rule;
                IdsSetIndex = step.IdsSetIndex;
                IdIndex     = step.IdIndex;
            }

            public override string ToString() => $"[{IdsSetIndex}][{IdIndex}]: {Rule}";
        }

        public void ParseReceivedMessages(string[] receivedMessages, out int messagesStartIndex, in Rule[] rules)
        {
            messagesStartIndex = 0;
            foreach (var line in receivedMessages)
            {
                messagesStartIndex += 1;
                if (line == "")
                    break;

                var rule = Rule.FromString(line.AsSpan());
                rules[rule.Id] = rule;
            }
        }

        public bool IsValidMessage(string message, Rule[] rules)
        {
            int charIndex  = 0;
            var rulesStack = new List<int>();

            bool IsValidForRule(int ruleId, bool endRule)
            {
                rulesStack.Add(ruleId);
                var rule = rules[ruleId];

                if (rule is CharRule)
                {
                    var charRule = (CharRule)rule;
                    bool isValid = message[charIndex] == charRule.Char;

                    Console.WriteLine($"[{charIndex}]{message[charIndex]} == [{string.Join(" ", rulesStack)}]{charRule.Char} {(isValid ? "OK" : "NOK")}");
                    if (isValid)
                    {
                        charIndex += 1;
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
                else
                {
                    var idsRule = (IdsRule)rule;

                    var tmpCharIndex = charIndex;
                    foreach (var ids in idsRule.Ids)
                    {
                        charIndex = tmpCharIndex;
                        for (int i = 0; i < ids.Length; i++)
                        {
                            int  id     = ids[i];
                            bool isLast = i == ids.Length - 1;

                            bool isValid = IsValidForRule(id, endRule && isLast);
                            rulesStack.RemoveAt(rulesStack.Count - 1);


                            if (endRule == false && charIndex == message.Length)
                                return false;

                            if (isValid && isLast && endRule && charIndex != message.Length)
                                return false;

                            if (isValid && isLast)
                                return true;

                            if (isValid == false)
                                break;
                        }
                    }

                    return false;
                }
            }

            return IsValidForRule(0, true);
        }

        public void Log(string message)
        {
            //Console.WriteLine(message);
        }

        public bool IsValidMessage2(string message, Rule[] rules)
        {
            if (message.Length == 0)
                return rules.Length == 0;

            if (message.Length == 1)
                return rules.Length == 1 &&
                       ((rules[0] as CharRule)?.Char ?? message[0] + 1) == message[0];

            var charsStepsStacks = new Stack<Step>[message.Length];

            charsStepsStacks[0] = new Stack<Step>();
            charsStepsStacks[0].Push(new Step((IdsRule)rules[0]));

            Stack<Step> CloneStack(Stack<Step> original)
            {
                var arr = new Step[original.Count];
                original.CopyTo(arr, 0);
                Array.Reverse(arr);

                return new Stack<Step>(arr.Select(s => new Step(s)));
            }

            CharRule GetNextCharRule(Stack<Step> stepsStack)
            {
                Log($"steps stack: [{string.Join(" ::: ", stepsStack)}]");

                while (stepsStack.Count > 0)
                {
                    var step = stepsStack.Peek();

                    if (step.IdsSetIndex >= step.Rule.Ids.Length)
                    {
                        stepsStack.Pop();
                        continue;
                    }

                    step.IdIndex += 1;
                    if (step.IdIndex >= step.Rule.Ids[step.IdsSetIndex].Length)
                    {
                        step.IdIndex = 0;
                        step.IdsSetIndex += 1;
                    }

                    Log($"Step: {step}");

                    if (step.IdsSetIndex >= step.Rule.Ids.Length)
                    {
                        stepsStack.Pop();
                        continue;
                    }

                    var nextRule = rules[step.Rule.Ids[step.IdsSetIndex][step.IdIndex]];
                    if (nextRule is CharRule)
                        return (CharRule)nextRule;

                    stepsStack.Push(new Step((IdsRule)nextRule));
                }

                return null;
            }

            void PopFromStackStepsThatAreAtTheLastId(Stack<Step> stepsStack)
            {
                while (stepsStack.Count > 0)
                {
                    var step = stepsStack.Peek();
                    Log($"Trying to pop step: {step}");
                    if (step.IdIndex +1 != step.Rule.Ids[step.IdsSetIndex].Length)
                        break;

                    Log($"Popped step: {step}");
                    stepsStack.Pop();
                }
            }

            bool AreAllStepsOnTheRulesLastIds(Stack<Step> stepsStack)
            {
                foreach (var step in stepsStack)
                    if (step.IdIndex +1 != step.Rule.Ids[step.IdsSetIndex].Length)
                        return false;

                return true;
            }

            const int limitOfLoops = 1000000000;
            int loopsCount = 0;

            int characterIndex = 0;
            while (true)
            {
                loopsCount += 1;
                if (loopsCount > limitOfLoops)
                {
                    Console.Write(" ======== FAIL =======");
                    return false;
                }

                bool isLast  = (characterIndex + 1) == message.Length;

                Log($"\n[{characterIndex}]: '{message[characterIndex]}' {(isLast ? "last" : "")}");

                var charStack = charsStepsStacks[characterIndex];
                var charRule  = GetNextCharRule(charStack);

                if (charRule == null)
                {
                    Log($"charRule == null, characterIndex: {characterIndex}");
                    if (characterIndex == 0)
                        return false;

                    if (isLast)
                        return false;

                    characterIndex -= 1;
                    //PopFromStackStepsThatAreAtTheLastId(charsStepsStacks[characterIndex]);
                    continue;
                }

                Log($"[{characterIndex}]: {charRule.Char} ?= {message[characterIndex]}");

                if (charRule.Char != message[characterIndex])
                {
                    // TODO(Arek): Co faktycznie należy zrobić gdy znaki się nie równają?
                    // Nie można zawsze pozostać na tym samym znaku i po prostu próbować
                    // nastęnej regóły.  To zależy czy się jest pierwszym znakiem w danej
                    // grupie, czy nie.  Poniższe rozwiązanie nie działa, ale nie psuje
                    // tak bardzo pozostałych testów.  Przedebuguj sobie na obecnie
                    // włączonym przykładzie.

                    var step = charStack.Peek();
                    while (step.IdIndex != 0)
                    {
                        characterIndex -= 1;
                        step = charsStepsStacks[characterIndex].Peek();
                    }

                    step.IdIndex = step.Rule.Ids[step.IdsSetIndex].Length - 1;
                    continue;
                }

                if (isLast == false)
                {
                    charsStepsStacks[characterIndex+1] = CloneStack(charStack);
                    PopFromStackStepsThatAreAtTheLastId(charsStepsStacks[characterIndex+1]);
                    characterIndex += 1;
                    continue;
                }

                /*
                var lastCharFirstStep = charsStepsStacks[characterIndex];
                if (lastCharFirstStep.IdsSetIndex +1 == lastCharFirstStep.Rule.Ids.Length &&
                   lastCharFirstStep.IdIndex +1      != lastCharFirstStep.Rule.Ids[lastCharFirstStep.IdsSetIndex].Length)
                   return false;
                */

                if (AreAllStepsOnTheRulesLastIds(charStack) == false)
                {
                    var step = charStack.Peek();
                    while (step.IdIndex != 0)
                    {
                        characterIndex -= 1;
                        step = charsStepsStacks[characterIndex].Peek();
                    }

                    step.IdIndex = step.Rule.Ids[step.IdsSetIndex].Length - 1;
                    continue;
                }

                return true;
            }
        }

        public int Solve1(string[] receivedMessages)
        {
            int messagesMatchingRulesCount = 0;

            int messagesStartIndex = 0;
            var rules = new Rule[1024];

            ParseReceivedMessages(receivedMessages, out messagesStartIndex, in rules);

            foreach (var rule in rules)
                if (rule != null)
                    Console.WriteLine(rule);

            for (int i = messagesStartIndex; i < receivedMessages.Length; i++)
            {
                string message = receivedMessages[i];
                bool isValid = IsValidMessage(message, rules);
                Console.WriteLine($"{(isValid ? "OK " : "NOK")} {message}");
                if (isValid) messagesMatchingRulesCount += 1;
            }

            return messagesMatchingRulesCount;
        }

        public int Solve2(string[] receivedMessages)
        {
            int messagesMatchingRulesCount = 0;

            int messagesStartIndex = 0;
            var rules = new Rule[1024];

            ParseReceivedMessages(receivedMessages, out messagesStartIndex, in rules);

            // Change rules
            // 8: 42 | 42 8
            // 11: 42 31 | 42 11 31
            rules[8]  = Rule.FromString("8: 42 | 42 8");
            rules[11] = Rule.FromString("11: 42 31 | 42 11 31");

            for (int i = messagesStartIndex; i < receivedMessages.Length; i++)
            {
                string message = receivedMessages[i];
                bool isValid = IsValidMessage2(message, rules);
                Console.WriteLine($"{(isValid ? "OK " : "NOK")} {message}");
                if (isValid) messagesMatchingRulesCount += 1;
            }

            return messagesMatchingRulesCount;
        }

        public void RunTests()
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

            void test(string[] rulesStrings, (string line, bool expectedAnswer)[] tasks)
            {
                int messagesStartIndex = 0;
                var rules = new Rule[1024];

                ParseReceivedMessages(rulesStrings, out messagesStartIndex, in rules);

                // Change rules
                // 8: 42 | 42 8
                // 11: 42 31 | 42 11 31
                rules[8]  = Rule.FromString("8: 42 | 42 8");
                rules[11] = Rule.FromString("11: 42 31 | 42 11 31");

                foreach (var task in tasks)
                {
                    bool isValid = IsValidMessage2(task.line, rules);
                    Console.WriteLine($"{(task.expectedAnswer ? "OK " : "NOK")}  {(isValid ? "OK " : "NOK")}  '{task.line}'");
                }
            }

            test(rules1, rules1tasks);
            Console.WriteLine();
            test(rules2, rules2tasks);
            Console.WriteLine();
            //test(rules3, rules3tasks);
        }

        public static readonly string[] PUZZLE_INPUT =
        {
            "18: 48 48",
            "25: 48 81 | 41 7",
            "48: \"b\"",
            "4: 131 48 | 70 41",
            "20: 61 48 | 57 41",
            "89: 41 41 | 41 48",
            "74: 41 107 | 48 124",
            "98: 41 48",
            "99: 97 48 | 92 41",
            "91: 34 48",
            "100: 48 41 | 67 48",
            "6: 48 100 | 41 132",
            "40: 81 48 | 7 41",
            "124: 83 48 | 130 41",
            "50: 7 41 | 7 48",
            "68: 64 41 | 24 48",
            "60: 30 41 | 86 48",
            "75: 89 41 | 39 48",
            "103: 67 67",
            "58: 41 22 | 48 111",
            "71: 67 34",
            "56: 34 48 | 39 41",
            "122: 48 120 | 41 89",
            "12: 41 18 | 48 98",
            "95: 34 41 | 103 48",
            "93: 110 41 | 34 48",
            "13: 43 41 | 69 48",
            "44: 101 48 | 114 41",
            "69: 106 41 | 32 48",
            "67: 48 | 41",
            "45: 7 48",
            "117: 48 120 | 41 39",
            "46: 48 29 | 41 82",
            "121: 48 49 | 41 47",
            "130: 103 48 | 89 41",
            "132: 41 48 | 48 41",
            "94: 41 4 | 48 76",
            "14: 9 48 | 93 41",
            "26: 41 72 | 48 81",
            "79: 67 1",
            "115: 67 132",
            "15: 41 20 | 48 63",
            "47: 120 41 | 81 48",
            "27: 100 41 | 7 48",
            "11: 42 31",
            "113: 49 41 | 56 48",
            "31: 48 133 | 41 127",
            "131: 90 41 | 28 48",
            "81: 48 48 | 67 41",
            "23: 84 41 | 27 48",
            "84: 18 48",
            "107: 79 41 | 33 48",
            "83: 67 89",
            "49: 81 41 | 7 48",
            "108: 102 41 | 60 48",
            "37: 41 7 | 48 120",
            "120: 48 41",
            "32: 41 96 | 48 95",
            "2: 48 85 | 41 128",
            "102: 48 62 | 41 50",
            "5: 41 110",
            "61: 41 37 | 48 6",
            "97: 41 120 | 48 34",
            "85: 120 41 | 89 48",
            "80: 120 48 | 98 41",
            "92: 48 103 | 41 34",
            "65: 16 48 | 58 41",
            "112: 48 71 | 41 123",
            "70: 87 48 | 115 41",
            "39: 41 67 | 48 41",
            "41: \"a\"",
            "38: 2 48 | 77 41",
            "110: 48 41 | 41 41",
            "88: 7 48 | 89 41",
            "52: 41 73 | 48 104",
            "96: 100 48 | 103 41",
            "66: 41 126 | 48 121",
            "77: 40 48 | 47 41",
            "3: 48 118 | 41 25",
            "126: 75 48 | 27 41",
            "1: 41 41 | 48 48",
            "19: 48 72 | 41 18",
            "42: 41 68 | 48 105",
            "129: 48 110 | 41 120",
            "72: 48 48 | 41 67",
            "7: 41 41",
            "59: 41 47 | 48 125",
            "73: 1 48 | 103 41",
            "114: 41 117 | 48 73",
            "118: 89 41 | 132 48",
            "51: 41 1 | 48 81",
            "101: 129 48 | 93 41",
            "133: 48 15 | 41 13",
            "104: 48 1 | 41 81",
            "123: 110 48 | 98 41",
            "0: 8 11",
            "55: 48 1 | 41 18",
            "30: 89 41 | 18 48",
            "76: 48 14 | 41 35",
            "43: 52 48 | 112 41",
            "24: 48 109 | 41 38",
            "29: 116 48 | 3 41",
            "106: 88 41",
            "9: 89 41 | 100 48",
            "125: 41 120 | 48 132",
            "22: 103 48 | 18 41",
            "21: 48 83 | 41 51",
            "64: 48 44 | 41 74",
            "111: 103 67",
            "54: 41 36 | 48 55",
            "119: 48 12 | 41 19",
            "35: 123 48 | 91 41",
            "127: 41 94 | 48 10",
            "116: 41 93 | 48 122",
            "8: 42",
            "128: 48 72 | 41 1",
            "28: 41 132 | 48 120",
            "63: 41 59 | 48 17",
            "87: 103 48 | 72 41",
            "36: 48 81 | 41 100",
            "17: 62 48 | 26 41",
            "62: 100 48 | 98 41",
            "34: 48 41 | 48 48",
            "82: 23 48 | 21 41",
            "78: 41 65 | 48 66",
            "33: 120 41",
            "109: 48 113 | 41 99",
            "57: 41 92 | 48 80",
            "86: 48 89 | 41 39",
            "53: 54 48 | 119 41",
            "10: 108 48 | 53 41",
            "90: 81 41 | 34 48",
            "16: 48 5 | 41 45",
            "105: 46 41 | 78 48",
            "",
            "bbaaabbbbaabbaababaababbbabbbaaa",
            "bbbbbaabbabaaaaabbaaaabbabaababbbbabbbbababaabaabaaaaaabbbaaaaab",
            "baabbaabababbabbaaaaabba",
            "ababbbbbbbabaaaaabaabbabbbbaaaabbabbaaba",
            "abbbbabaabbbabbbabbaaaaaaaaaabba",
            "bbaaaaaabaabbbababbaabbabbabaaaabbbaababaaaabbbaabababaa",
            "bbbbbaabbabbbbabaabbbbaaaaaaabba",
            "bbbaabbaaababbbbabbbaabb",
            "abbbbaaabbbaabababaababa",
            "aaaababbabaaabaaabbbbbaaaabaaabaaababbbbbaabaabababbbbbbbaabaaaa",
            "babaababaabbbbabbabbbbabbababbaa",
            "babbaaabababbaaabbbaabab",
            "aaaabaabbbababbbbbbbbbbb",
            "aabbbbbabbbbabbbbbbbbababbbabababaaaababbbaaaabaaaabaaaaabbaabbaaababaaaabaaabab",
            "aaabbaabaaaabbbababbbbbbbabaaabaababaabb",
            "abbaaaaabbaabaabbbabbbab",
            "baabbbaabbabbabaabbabbbb",
            "abababbbaaaaabbbbbbbbabaabaababbabbbbbbbbbabbabbaaabaaba",
            "abbaaaaaaabbabbaaaababab",
            "baabababbaaaaaaabbbbabab",
            "babbabaaaaabaabbbbbaaaab",
            "aabaaaababbaaabbbbbbaaaa",
            "babaaaaabaaaababbbabbabb",
            "abbabaaaabbababbaaabbaba",
            "aaabaaaaaabaaaababaaaabaaaaabaabaababbabaabababa",
            "ababbabbaaabaabababaabbaaabbabababaabbaa",
            "baaaabbbbbabaaaabbbabaabbabbbaaa",
            "abaabaaaababababbaabaabb",
            "aabaabbbbabaaabbaababbaabbbbbbabbbbaabababbabbaa",
            "abaaaaaabbaaaabaaabbaaab",
            "aaaabbbbbabaabaaababaaabbbbbaaabbbbbabbbaaaabaaabababaaa",
            "bbabbbbaaababbbbaaababbbbbbaabbaababbabbaaaabbbaaaaabbbaaabbabbbaaababab",
            "babaaaaaaabaaabaabbbabab",
            "aaaabbaaabaaabaabaabbbabaaaabbbbabbaaabbbbabbbbbbbaabbabbbbbababababaabb",
            "abbbabbabbabbbbaaabaaaaababbaabbaabaaabb",
            "abbaaabbbbaaababbbbaaabbabbbbbbbbbbbbaababbbbbbbabbbbabbabbabbbb",
            "babbbaababbaabaaaabbbbbb",
            "baaaabbbbbbbaabbbabbbababaaaabba",
            "aaabbbaababbaaaabaaaabbbbaaabaaa",
            "abbbbaaaaababbabbbabaaabbbaaabba",
            "babbabaabaaaabbbbbbaaaab",
            "abbababaabbbbaababaaabba",
            "aababbbbbababaababbaababababbbbbbaaabbbbaaabaaab",
            "baabbaabaaabaabaababbaaaabaaaabaaaababbbbbbabbbbbbbbabab",
            "bbababbaaaabaaaaabbabbaa",
            "bbababbbbababbabbbaababbbabaabbaabbaaabaabababaa",
            "baababbabababbabaababbab",
            "abbababbbaababababaaabaaababbbabbbbaaaab",
            "abbbbbaaaabaaaaaabababbb",
            "abbbbaaabaaabbbaababbbaa",
            "bbababbbbabaabaabbbbabaaaaaaaabb",
            "aaaabbaaaabbbbaaaaababba",
            "abaaabaaaababbabbaabaaba",
            "babbabababbaabaaabaaabbb",
            "babababbbaaaabbbbabaababbbbaaaaaaabbaababbaababbaaabaaababaaaaabbbabbbab",
            "baaabbabbaababbaaabbbaaa",
            "bbbababbbabababbaaaabaabababaabaaabbabbb",
            "bababbabbabbbbabaababbbbabaabbabaababaabbbaaaabaaaabbaabaaabbaaa",
            "bbabbbaabaaaabbbbbabaaabababaababaaababa",
            "aabbbbaabaaaaaaaaabbbaabbbbabaababbaaabbbabbabbbaaabababbaabbaba",
            "bbbbaabbbabbabaaabbbbbaaabaaabab",
            "baabbabababaabbbabbabbbaabaabaabaabbbbaabbabbabbabbbaaaabaaaaabaabbbaaba",
            "ababbabbbbbaaabbbbbbaaaa",
            "aaaaaabbababbbabbaababbb",
            "ababbabbaaabaababaababaabaababaaababbababaaaabaa",
            "baaaababaabaaaaabaabaaba",
            "bbabbbbaabbbaabbbabbaaabbababaababbbbbabbbabbbbb",
            "bbbababbbaabbbaaabbbbabb",
            "bbbababbbbbbbaababbbaabbbababbbabbabbabb",
            "abaaaaaaaaababbbaaaabaaa",
            "baaaababbbababbaaaabaaab",
            "babaaabbbbbabbaaaaababbbabbbabaaabbabbaa",
            "bbaaaabbbaaabaaaabababbbbaabaaabbbbbbbbabaaaaabbababaaaaaaaabbabbbaaabaababbbaab",
            "aabaaaabbbabbbaaaaaaabaa",
            "ababaaabaabbbbabbbbbaabbaabbabbababbbbbbaabbbbbb",
            "bababaabbbbabaabbababbba",
            "bbbbbbabababbabbbbaaaaaabaabaaaaaababaaabbbbbbaabbbabbbaaaaaabaa",
            "abbbbaaaababaaabaaabaabaabbbbbbbbabbbaaa",
            "abaabaabbabbaabbbabaabaabbbabbbbabbbaaabaabbabaaabbabbaabbaaabba",
            "abbbbbaaababbaaaabbaabaabbabbaaabbababbbaaaaabaaababaabb",
            "abbaaababbbabbbbabbbbababaabababbbbbbbabababbababbbbabbb",
            "aababaaaaabaaaababbbaaabbbabbaabababbabababbabbb",
            "bbbababaaabababbabaabaaabbabbbab",
            "bbabababbbaabbababbababaabbaaaaaaaabbaba",
            "aaabaabbbbaaabbbabababbb",
            "abababababbbaaaaabbababbaabaaaaabbaaababaabbbababbabbbabaaaaabaa",
            "bbbbabaaabaabaabbbaaaabaaaababbbbbaababababababa",
            "aaaabbaaabaababbbaaabbbb",
            "baaabbabbabbababaaaabbabaabaaaabbbbaabbbabaaaabb",
            "aabaaaaaaaababbbabbbaabaabbbbaabbbbbbbabaababababaababaa",
            "babaabaaaaaaabababbaaabbbbbaabaabbababab",
            "aababbbbbbbabaaaaaabaabbbbbabbaabbabbababbbbaaba",
            "bbbabbaababbbaabaabbaaaa",
            "abbbaabbbbbabaaaaaababab",
            "babbababbbbabbbbaabaaabaabbaabbaabbabaab",
            "abbaaaabbbabbbaaabbbbbab",
            "aabbabbbbabababbaaabbbbaaababaabbabbbbbbbbaabbbaabbbabaababbbaabbbaabbaabbbaabbb",
            "bbabaabaabaabaaaabbababaaabaabaa",
            "bbbbbaabaaaaaabbaabbbaababaabaaabbabaaabbbabbaaaabaaaabb",
            "aababbababaaaaaaabbbbbbbbaabaaab",
            "aaabbbaabbbaabbababaabbabbbbabaabbaabbaaaabababbabbbbbba",
            "bbbbbababbabbaaabbbabaabaabbbbbb",
            "aababaabaaaaaaabaabbaabaaaabbaaaaababbbbabbbabbbbaaabbab",
            "baabbaaabbaaaabbaaabbbbbabaabbabbbbabbba",
            "bababaabbabbbbbabaabbbbabbbaaaab",
            "ababbaaabbbaabbababbbbab",
            "babaaaababbbbbbbaaaaaababaabbbba",
            "aaaabbabbaaabbaabaabbaba",
            "bbbbaabbbbbbbbababababbabababbaa",
            "baaabbaaabbaaaaababababa",
            "aaabaaaabbbbbaababbbbababbabbbaaaabbabbaababaabbbaaabaabababaababbabaabb",
            "babaabbababaaabbbababbbb",
            "baabababbbaaaabbbbabbaaaabbabbaabbabbbbb",
            "babbbbababaaaabaabbbbbaaababbaabaabbbbbabbaabaaaaabababb",
            "babbbaaabaabaaabaabaaabb",
            "aaabbbbbbbaaabbbbaaaaaba",
            "baaaaaabaabbbbbbabaababa",
            "ababaaabbbbaaabbababaaaaababbaababbbbaaaaababaaabbabbbaababababbabaabaabbabbabbbbaabaabb",
            "abaaaaaaabbaababbbaababbbbbaabbbbbaabbaa",
            "ababbbabbabbbaabbbbbbbbb",
            "abbaaaabbaabbbababaaaabb",
            "bbababbbababbbababbabbbb",
            "baabaaaababbabaaababaaabbbbaaabbbbbbbbbaaabbaaaaaaabbbba",
            "baaabbabbaaaaaaababaaaabbaabbbba",
            "abaaabbbbaabbaababababbaaabbbaaabaababab",
            "bbbaabbaaabbabaababbababaabbabbbbabbaabaaaabbaabaabaabaabaaaabbb",
            "aabbaabababaababaaababbbbabaabbb",
            "abbaabbabaaabbabbaabaaab",
            "bbbbabaababbbabaabaabbbb",
            "baabaaaaababbbabbbbaaaaabbbabababaababaa",
            "baaaababbbaaababbbabbaabaaabbaaa",
            "babbabaabbaabbabbbabbabaabaaabab",
            "aababaabbabaabbabbbabbba",
            "aaaaaaaaabbaabaaaabaabbbabbabbab",
            "babbbaabbbabbaaabbabbbbabaaabbbb",
            "abbaaaaaabbbabbababbbbbb",
            "babbabbabaababbaabbbabbbaaababaa",
            "bbabbbaaaabaabbbaababaabaababaabaaaaabba",
            "babbabaaaaabbbbbbbbabbba",
            "baaaaaaabbbbbababbbbbbba",
            "bbbbabaaaaaaabbbababbbba",
            "aababbabbabaababbaaaabbbbaaabaaa",
            "babaabbabbaaaabbbaabaaba",
            "abbbaaabababababbbabbbbb",
            "bbbbaabbababbabbababababbbbaaababbbaaaba",
            "aabaaabaabbabababbaaaabbababaabbbbaabbba",
            "bbbaabbabababbabbbbbbbbb",
            "abbababbbbbbbaabbaaabbababbbbaabaabbaaaaabbbbbab",
            "ababbabbbbaaabbbababbabbaaaaaaba",
            "bbabaabababbbbabbababaabaabaabbaaabaabab",
            "bbaaabbbbabaabbaaabbbaba",
            "baaaabbbbaabbaaaabaaabaabaababaabbbaabaa",
            "aababaababbaaaaaabaaaabb",
            "bbbbabaababaababaaaaababbbbbabaababaababbbbaaaab",
            "abababbbbababbbbbaaaabbbaabaaaaaababbabababbbbbbbbaabbba",
            "bbaaaaaaabbbbaaabaaaaabb",
            "bbabaaaaabbabaaababaaaabaaabbaab",
            "bbbbbababaabbaaaaaaaaaaaabaaaabaabaabaabbaabaaab",
            "baabbbabaaaaaaaabbbabaaaababbabbaaababab",
            "aabbaabaabbabaaaaabbbbaabaaabbababbabbba",
            "aaaabaababaaabaabbaaaaab",
            "abaabbabababbbbbabbbabbabaabbaababababaa",
            "bbbbaabbbbbbaabbbaabbbaaaabbbaabbaabbbababbbabababbaabbb",
            "bbaabbaababbaabaabaaaabaaaabbaaabbababaabbaabbbbbabbaaaabbbbbbababbbababaabbaaaabbaaabbb",
            "aabbbbbaaabaaaaabbaaaaaabbabaabbabbaaaabbbbbbbbbbbababbbbbababab",
            "bbbbaabbbbbaaabaaabbabbabbababbaababaabb",
            "abaaaabbaaaaaaabbbbaababbabbbbaabbababaaaabbabbb",
            "bbaabaabaabbabbababbabaaababaababaaaaaba",
            "bababbaaabaabbabaaababaaaabbaababaabbaabaabbabbababaabbabaaabaaaaababbabaabaaabb",
            "bbbaaabaaaababbbbbaabaabbaabaaba",
            "abbabaaabbaaaaaabaaaabaa",
            "bbaababbbbaaababbabaaaaaaabababbaaabbabb",
            "abaabaaaaaabbbbbabbabaab",
            "abbbaabbaabaabbbbaaaaabb",
            "abbaaaabaaaaababbaaaabbbababaaabbaaabaaa",
            "bbbaaabbbbaabaabbaaaaaab",
            "aabbbbaababbbabaabaabbba",
            "aababbbbbaabbbaaaaaabbabaababbba",
            "abaaaaaaaabaaaabbabaababbbababbaaabbbaabaaaababbbbbabbba",
            "bbaaaabaabbbbaabaaababaababbaabbbabbbaaa",
            "aabaabbbbaaaabbbaabaabbbbbabaaabbbabbbbaaabbbababababbbbbaabbbbb",
            "aabbaabaabbaaabaaabaaaab",
            "bbbaabbbabaaaaabaaaababb",
            "babaaaabbbbabbaaaaaabaabaaaababa",
            "baaaabbbbbaababbbbaabbab",
            "bbabbbbabbabbbbaabbbbabb",
            "baaabbababbbaaaabaabbabbaabbabbb",
            "abaabaaababbbababbabbaabaababaaaaaabbbbabbbaabaaaaabbaaaababbababbbbbbaa",
            "aabbbaabbabbabaaabbaaaaaababbaaaababaabaabababbbaaabbbbaaabbabab",
            "babbaaabbaaabbababaaaaab",
            "abbaaabaabbaaaabaabaaabaababbaba",
            "bbaabbbbaabbaabbbaaababa",
            "aabaabbabababbbbabbabbabaaabaaab",
            "ababaabbbbbbbaaaababbaba",
            "abbbbaaaabaabbabbaabbabbaabbbabb",
            "babbabaaaabaaaabbbbbabab",
            "bbaaaaaaaabaaaaaababaaaa",
            "aaabaabbaaabaabbbbaaababaaababbbaaabaaabbababaaa",
            "bbbaaabababaaaabbabaabaabbaabaabbbbbaaaa",
            "aaaabbbbabbaabbaaabbbbbb",
            "abbabaaabaabbbababbbaabbbabaaaaaaaabaabbbbbbbabb",
            "bababaabbaabbaaaaababbbbaaababababababbb",
            "bbbbaabbabbbbaabbbaaaaaaaabbbbaababaabaaaabbaaaabbbaabbb",
            "aaaaaaabbbbbaaabbabbaabaababbababababbbaaabbabbbbaaaabaa",
            "babbababbabaabbabababaaa",
            "aaaabaabbabbbbaabbbbbaabbaaaaaabbbaaaaabaabaabbbbabbbabbabbaabba",
            "aaaabbabaabaaaaabaabaaaaaaabaababbbaabaa",
            "aababbaaabababbaaabbaaaa",
            "baabaaaabaaabbbaabbbaabbbbbaaaab",
            "baabbbabaaaabaabbabbababbabaaaabaabbbabaaaabaaabbbbbbabb",
            "ababababbbaaaaaaababbbbbbbbabbab",
            "aabbbaabbaaabbaabaabbaaaabbabaaa",
            "bbbaabaabaabbabbaabaaabbbbbaabaabaabaabbbbabbbaabaabbbbbbbbbbaaaabbbabab",
            "bbaaababaaaaaaaabbbaabbb",
            "abbbbbaababbbbabaabaaabaaabbbbaaaabbaabababbaaaaabaaaabbbbbababa",
            "abbaaabababaababaabbbbaaaabbbbaaababaaaa",
            "abaaaabaabbaaabababababa",
            "abbaababbaaabbbaababababaabbaabbbbaaabaa",
            "abbaabaabaaaabbbbbbbaabbaaababbbababaaba",
            "abbbbbaabaabbbaaaaaaababbaaaaabb",
            "abaabaabbbaabaabaabaabab",
            "aabaabbbabbaaaabaabbbaba",
            "bbbbbbababbbbbbbbbababab",
            "babbbbabbabbbbabbbbbaabbbbbbaabbaaaaaabbbbbaabaa",
            "bbabbabbbbbbbabaabbaaaababbabbababaabbaabbbbbbaaabbbbbbaabbabababbabaababbaaaaba",
            "abaaaabaaaababbbaaabbbbbabbbabbaaaababaa",
            "aababaaabaaabbababaaabba",
            "abbaabbabbabbbaabaaaaabb",
            "aababaabbbaaaaaabaabbbaabbaabbbbabbbbaaaaaaababb",
            "abaabbbbaabbabaabaaabbaaababaabaabbabaab",
            "bbbbaaabbabababbbbbabbaabbbbbabbababaaaa",
            "bbbbaabbaaabaabbbbbbbabb",
            "abbbaaabbaabababbabbbaaa",
            "abbaabababbbabbbbbaaabbbaabaaaabbbbaaababbbbabba",
            "bbbababbabbbbaabbabaabababbbaabaaabbaaab",
            "bbabbbbbabaaabbabbbaaabbbbbaaababbbaababaabaabbaaabbaaabbabaaabbbbbbabbaaabbbaab",
            "aabbbaababbababababbabaabaabababbbaaaaab",
            "babbaaababababbabbbbbbba",
            "baabbaaabaaaababbbabbaabbbabaababbbbbbaaaaaaabaa",
            "abaabaabbbaabaabbaaabbabaabbbbaabaaabaab",
            "babbbbababbbbbbbaaaaabaa",
            "babaabbaabaaaaaabababbaa",
            "aabbbbaabbabbbbaabbbbbaabbbbbababaabbbabbaabaaabbbaabaaa",
            "abbabbaaaabaaaaaababbaaaabababbbaaaabababaaabbaa",
            "baaaaaaabbaababbbbabaabaababbaabbbaaaabaaaababab",
            "bababbabaabaaababbbaabab",
            "aaababbbaaabbbaaaaabbbab",
            "aabaaaaabbbabaababaaaaab",
            "aabbbbababbbbaabababbbabbaabaabb",
            "bbaababbbbababbaaaababaa",
            "baaaaaaabbaabbbbbaabaaba",
            "aabbabaabaabbaabbbbabbbbaaabbaababbabbbb",
            "bbbbbaabbabaaaaaabbabbab",
            "bbaaaabbaaaabbbbbbbbbbbbbabababa",
            "baaabbaabbabbbbaaabaaababbbbaaabbabbabababaaabab",
            "bbbababbabbbabbbabbaabbababaabbb",
            "bbabaababbbbbabaabaaaabaabaababa",
            "bbaababbbbbbbbbabbabaabbbbaababa",
            "abbababbabaabaabbaaabbabbbbababababaabbb",
            "aabaabaaaabaaaaaaaaabbabaaababbbaaabbbbaabaaaaab",
            "babbbbabababbbaabbaabbbaabbaababaabbbbbaabbbbbbabbaaaaaaaaaaaabbabababbb",
            "bbabaaabbaabbaaaaabbbaba",
            "ababbbbbbbababbbbbaaabbbaaababba",
            "bbbbbbabaaaaabababbbbabb",
            "bbbababbabbbbaabaaaaaabbaaaabbbbaaaaaababbaabbab",
            "bbabbaaaaaaabaaaaabbabbb",
            "babbaaabbbbaaaabbaababbbabbbabaababbbbbabbbabbbaaabbbbaaaaabaaaabaaabbbb",
            "bbabbaababbaabbabaabbaaaaabbbbbbbbbabababbabbbababababaa",
            "abbaaababababaabaaaababa",
            "aaabbbbbabaabbababaaaabb",
            "ababaaababaaaaaaaabbabaabbabbbbbbabbbaaa",
            "aabababbbbaaabbaaaababababababbb",
            "baaabbbaabbbbaabbbabbbbababaaaaaabaabaabbbaaabbaabbabbba",
            "aaaabbababbbaaabababbbabbbabbaba",
            "bbaabababbbaabaaaaaaaaaababbaaabbbbababbabbaaaabababbaaababaaabbaababaababaaaabaaaaabbaa",
            "aabbaababababbabbaabbaba",
            "aabbaabbaabaaabababbbbabaababbabbbaababbbabbbbaa",
            "ababababaabbbbbbbaaabaaabababababaaababbaaaaabbababaabaaaaaaaabaaababaab",
            "baabaaaabaaaabbbabbbbbbbabbabbaabbbaabab",
            "abababbabbaaaaaaaabbabbabaabaaba",
            "baabbbaababaaaabbbaababa",
            "abbbabbaababaaababbbbbab",
            "bbaaaabaabbaaabbbbabaaabbabbbbbbbbbabbabbbaabaaa",
            "baaabbaaabaaaaaababaabbb",
            "baaaaaaaaaabaabbaabbbbba",
            "bbbbbbabaaaabbbbbbbaabbabbaabbba",
            "bbbbbababaaababbabbbbaabbabbabbbaabbaaaabaaaabaaaaaabababbbababa",
            "aaaaaabbbabaaaabbabbaaba",
            "aabaaaaaababbbbbaababaabbbbabaabaabbbbaa",
            "bbaabaabababbbabbabbabbababbabababaaabab",
            "baabbbaababaabaabaaabbaaaabaabbababaabaaabbbabaa",
            "bbbababbbabbaaaabbabbaaaaabbbbaababababbaaaababb",
            "ababaaabaabaabbbbaaaabba",
            "abaaaaaabbbababbbaaaabba",
            "aaaabbababbbaaaabbabbaabbbaaabbbabbbbababbbaaaab",
            "babaabbaabbbbaabbbbaabbb",
            "bbaaaabaabbbbaabbbababbbaaaabbabbbaabbab",
            "aababaabaabbbaabaabbaaaa",
            "aaaabbbbbabbabaabaaaaaab",
            "aabbabbabbbbbbabaaaababa",
            "bbabbbbaabbbabbbabaabaabababbbbbabbabbbb",
            "bbbaabbababbabbaababbaabbbbbabaaabbaaabbbaabababaabbababbbbabbbabbaabaaabbbbaaba",
            "bbbabbaabbaababbbabbbbabaaaabbbbababbaaababaabbb",
            "aaaabbabbaaaababaabbbabb",
            "abaabaaababaabbaabababbaababbaaabbbaabbaabaaaabbbaaaaabbbbbaabbbaaaabaaa",
            "bbababbabbbbaaabbabbbbabbbabaaabaaababaa",
            "baabbaaabbbbabaabaaaaabb",
            "aababbaaababbbbbbaaabbbaabbbabab",
            "aaaabaabbbabaaaabbabababbbaabbbbbabbaaaabaaaaaab",
            "babaabbaaababbaaabaabbaa",
            "abaaabaaabaaabaaaababaaaaaaaabbb",
            "bbbbaabbaabbbbaabbbababa",
            "bbaaaabbaabbaabbbbbabbba",
            "aabaaaababbabaaaabaabbabaaaaaababbbbbbbb",
            "aababaabaabaabaaababbaba",
            "bbaaaabababbabaaaaaabbbb",
            "babbbaabbbbababbbbabbaabaababaaa",
            "baaabbbaaaabbbaabbaababa",
            "abbbbbbbbbbabaabbabbabbb",
            "aabaabbbaabbabbababbaaabaabbabbbabbabaab",
            "aaabaabbaabbbbaaabaababa",
            "abbababbaabbbbaaaaaaaaaaaaabbbbbbabaabbb",
            "aababaaaaabbbaabaaaaabbb",
            "aaaabbbbbbbbaabaaabababaababbbaa",
            "aabaabaaabbaaaababbabbab",
            "aaabbbaabbbbabaaabaabbabbabbaaaaababaaabaaabaabbbaaabaab",
            "bbaaabaaaabbabbbaabaaaaababbababbbabbbaa",
            "bbaabbbbaabaabbaabbbbbab",
            "baabbbaabbaaaabaaaaababa",
            "aaaaaaaaabaabaababbaabbb",
            "abbbbababbaababbbaababbaabaabbabbabaabbabaaaaaab",
            "aabbbaabbbabbbaaaababbba",
            "ababbaaaaabaabbbabababbb",
            "aabbbaababbbabbbaababaaaabbbbababbbaabaaabaaabbbbbaabbaa",
            "bbbaaabbaababaabaabbabbb",
            "baaaaaababababbaaaabbbbaaabaabaa",
            "abbaaabababbbbabbbbaabbaabaabaabaaaaaaabbaaabbbb",
            "baaaabaaaabaabaabaaabbaabaababbaabbbbbabaabaaababbbabaabaaaababb",
            "ababbabbbbbabbbbaababaaa",
            "bbbaaabbbababbabbaabaabb",
            "babbbaabbbababbbbbbabaababbbbabbbbbbabbabbaaaabaabababbabbbbbbbbabbbaaabaaabbbab",
            "bbbaabbababaababbaabbbba",
            "aabaaabbbbabbbbbbaabbbaababbaaabbaaaaabaaaaaabbbbbaaaabbbaabababbabbaabaabbabaaaaabbabab",
            "aababbbbabaababbbbaababaaabbaaab",
            "bbaaabaaabababbaaabbababaaaabaaabaabbaaabbaababaaaaababababababbbaaaabbabaaabaaaabbaabba",
            "abbaababbaaababbbbbaaaababbabbbbbbabbaba",
            "aaaabbaaaababaaabaaabbbabbabaabababaabbb",
            "baabaaaababbabaaaaabaabbbaaaaababbbbabba",
            "babbababbbbabbbbbabaaaaabaabbbaaaaaabaabaabbbbabbbaaabaa",
            "babbbbabbbbbbbabaabbbaaa",
            "abbaaaababababbaababaaabbaabbbabbabbabaabbaabbabbbbbabbb",
            "bababaababbbaaabbabbaaba",
            "bbabbbbaababaaabbaabbbaabbbaabbbbbbaabaa",
            "abbaaaababbaaaabbabaaabbbbabbaaabaaaabababbbbbbaaaababaaabaabbaabababaaa",
            "babaaaabaaaabbbbbaabbbabababbaba",
            "abbaaaaaaabaaaaaaaababaa",
            "aabbbaabaaaaabababaaaaab",
            "baaabbbababaaaabbbbbabab",
            "babbbabaaababaaaaabbbabb",
            "bbabbaabbbaaaabaabbaababbbbbabba",
            "abaabaabbababaabbababbabbbabbaaabbaabaaa",
            "aabbaabaaababaaabbababab",
            "bbababbaaaabaaaaaaaabbbbbbbaaaabbbaabbab",
            "bbaababbbabbaaaaababbbabbbaaabba",
            "bbbaaaaabbbabaabaabaaaaaabaabbbaaabbaaab",
            "babbbababababbabbabbbabb",
            "bbbbbbabbbaaabbbbabaabaa",
            "bbaabbbbbbabababbbbabbabbaabbbbaaabaabbbbbabbbbbbaabaaaabababbbaaabbbaaa",
            "bbbbbababbbabbbbabbbbbaaabbaabaaabbbabaaabababbb",
            "aaaabbabaabbaaabbaaabbbabbabbabaababaaabaaabbaabbabbbbbabaabbabb",
            "babbaaabbbabbbbbbbbabbbbbbbaabbaaabaabbabbababaaaaaabbaa",
            "abbaababababaaababaaabaaabaabaabbbababbabbbbbaaaaaaabbba",
            "bbbbbbabbabababbabbbaabbaababababbaabbababbbbbab",
            "baaaabababbaaabbbaabbaba",
            "aababbbbbaabbbabbaaaaaba",
            "abbbbaaaababbabbabbaabbaabaabbba",
            "bbbbbaabbabaaabbbbaaabba",
            "babaabbabaaabbabbbaaaaaaabaaaababaaabbba",
            "bbbaabaaaabbbaaabbabaabb",
            "aaaabbabaabaabbaabaaabba",
            "bbbabbaaaabbaababaabaaba",
            "bbabbbbaabbbbbbbabababbabbbababa",
            "baabbbabbabbaaabbbbaabab",
            "abbaaaaababababbabaaaabaaabbbabaababbbba",
            "aaabbbaabababaaaaabaabab",
            "baaabbaabababbabbaababbb",
            "abbaabbaaabbbaabbaaabaaa",
            "ababaaabbabaaaaaabbaaaaabbaabaaa",
            "ababababbbaabaababbbbbba",
            "bbbbaaabbbabbaaabbbaaababbabbaaabaabbabbabbbabab",
            "ababbaaabababaabbaababbaaaabbbab",
            "abbbabbbaabbabaaaaaabaabbbbbbabb",
            "aababaabbbaaaabbabbaabaababbbbbbbbbbbbba",
            "bbbaaabbbbbaaabbabbbabaa",
            "abbabaaaababbaaabababbaa",
            "babbbabababaaabbbbbaabab",
            "bbbabbbbbabaabaaabbbabab",
            "aababbabbabbaaabbbbabbaaaaabbbba",
            "aabababbbbbbaaabababaababbbaabbabbbababbabbabaabaabbbabbabbbabbb",
            "aababbabbbabbaaaaabbaaaa",
            "bbbabbaabbbbbabbaababababaabaabb",
            "baaaababaabbaabbabbaaabbbaababbabbbbabab",
            "aababbaaabbbbababbaababa",
            "bbabaaabababbbabbaabaaaababbaabb",
            "abbaaaabaaaaaabbaabbabbb",
            "babaaabbbaabbbabbbabaaaabaaaaaaaabbbabaa",
            "aabaabbbbbaabaabbbabaaaabababbba",
            "abbbbababaabaababbbbabbbabaaabbbaaaaabbbabababaabbbaabaa",
            "baaabbbabbaabaabababbbba",
            "bbbaaaaaabbbbbaabbabaaabbbbaaaab",
            "aababaabaabbbbaaaabaaabb",
            "aabaabbbbabaababbabbbabaabababbb",
            "bbabaaabbaaaaaaaaaaabbbbaaaaabaa",
            "baaabbabbbbaaababbaaabbbababbaba",
            "abaabbabbabbabbababaababbabbbabbbbbbbbbb",
            "bbaaaaaabbbbaabbbbbbaaaabbaabbaa",
            "aabbabaabbababbabbbaaaab",
            "baaabbbbaaababaabababbba",
            "bbababbabbbabbaabbaabbbbabbaabbbbbaaabba",
            "baababbaaabbaababaaaabbbbaabbbba",
            "baababbababbbaababbbaabbbbabbaababaabababbbabbba",
            "baaabbbababbaaabaabaaabb",
            "babbabbaaabbbbbbabaaabbb",
            "bbbaaababbbabaaaababbbbbbaababaa",
            "aaababbbaaabbbbbbbabaaabaaabbbaabbaabbbaaaaabbbabbababaa",
            "bbababbababababbbbaaabaa",
            "babaabbaaaabbabbbaababbbaaababab",
            "baaabbbaabbaaababbbbbabb",
            "bbaaabbbbaabbbabbaababbababaaaabbaaaaababbaaabaabababbba",
            "aabaaaababaaabaababaaaabaaaaaaba",
            "aabbbbaaabaabaabababbabbbaabbbaabaaabaabaaaaaabaaababbba",
            "bbabbaabbbbabaaabbbaaababbbaaaabbbababab",
            "aababbbbababaaaababbabba",
            "babbaaaabbbbabaabaababbbababaaabaaaabaabbababbaabbbabbaa",
            "bbabbaaabbabbbbabaabbaaa",
            "bbabbbaabbaaaabbaaaababb",
            "bbabbbbaabbbbababaaaaaaaabbaabbb",
            "abaabaaaaaaaaabbabbabbbb",
            "aabaaabaabababababaaaababbbbbbababaabbbbbbbbbbbbbaaabbbb",
            "babababbbbaabaaababbaaababaababa",
            "abbbbaaaaabaabaabaababbaababbbbabbabbbab",
            "aabaabaaabbbbaaaaaaaaaaaaababaaabbabaaabababaaaaababbaba",
            "aaaabaabababbabbaaabaabbabaababbbaaaabbbbbbababa",
            "bbbaabbabaaabbabbababbba",
            "ababbbabaababbbbbabbbbba",
            "abbaaaaabaabbaaababbbbabbbbbaaababaaaabbaabbabab",
            "baabbaabbbabbaaaabababbb",
            "abbbaaababbabaaababaabaabaaaabba",
            "ababbaaaabbaaaaabbbbaabbaaabaaaabaaaaaabaaaabaaaaabbaaaababbbbaa",
            "abaaaaaabbaaaababababbabababbabbbbaaabaabbbaabaa",
            "aabbbbabababbbbbbaabbaba",
            "bbbababbbaaabbaaabaabbba",
            "abbaabaaababbaaaaaabaabbbbaaaabaabbbaabaaabaabab",
            "bbabbbaabaaabbabaaabaabbbbabbbaabbbaababbaaaaabbaabbbbba",
        };


        public static readonly string[] PUZZLE_INPUT_IREK =
        {
            "62: 109 24 | 103 36",
            "9: 75 36 | 67 24",
            "66: 107 24 | 91 36",
            "0: 8 11",
            "44: 24 34 | 36 36",
            "7: 36 24 | 24 36",
            "74: 114 36 | 92 24",
            "128: 36 103",
            "52: 24 98 | 36 98",
            "20: 108 24 | 26 36",
            "75: 114 36 | 125 24",
            "33: 24 125 | 36 98",
            "110: 34 124",
            "102: 24 78 | 36 97",
            "27: 82 24 | 110 36",
            "23: 36 100 | 24 3",
            "43: 57 24 | 103 36",
            "49: 24 5 | 36 61",
            "85: 24 6 | 36 93",
            "71: 60 36 | 90 24",
            "55: 24 50 | 36 80",
            "10: 24 65 | 36 64",
            "16: 33 36 | 132 24",
            "47: 48 36 | 112 24",
            "125: 34 34",
            "8: 42",
            "14: 24 37 | 36 2",
            "56: 97 36 | 15 24",
            "25: 36 127 | 24 38",
            "100: 24 114 | 36 68",
            "101: 115 24 | 122 36",
            "54: 36 98 | 24 92",
            "60: 34 114",
            "19: 24 36 | 36 36",
            "113: 12 36 | 69 24",
            "29: 109 24 | 125 36",
            "59: 36 19 | 24 57",
            "81: 24 109 | 36 124",
            "119: 36 74 | 24 90",
            "96: 36 76 | 24 4",
            "109: 36 34 | 24 36",
            "93: 36 36",
            "11: 42 31",
            "39: 17 24 | 129 36",
            "132: 36 114 | 24 44",
            "6: 36 34 | 24 24",
            "127: 44 24 | 124 36",
            "114: 24 24 | 24 36",
            "73: 125 24",
            "58: 36 51 | 24 40",
            "21: 23 36 | 56 24",
            "5: 98 36 | 19 24",
            "105: 36 103 | 24 92",
            "38: 93 36 | 92 24",
            "82: 24 103 | 36 92",
            "24: \"b\"",
            "15: 36 109 | 24 6",
            "97: 36 44 | 24 7",
            "50: 98 36 | 124 24",
            "69: 98 36 | 57 24",
            "63: 47 24 | 13 36",
            "48: 24 120 | 36 116",
            "17: 105 36 | 43 24",
            "22: 94 36 | 66 24",
            "95: 36 27 | 24 45",
            "120: 24 103 | 36 125",
            "57: 36 36 | 24 24",
            "99: 24 101 | 36 119",
            "53: 24 96 | 36 89",
            "115: 36 98 | 24 114",
            "103: 24 24",
            "94: 24 83 | 36 79",
            "111: 24 19 | 36 93",
            "123: 36 114 | 24 98",
            "18: 36 19",
            "116: 36 125 | 24 93",
            "79: 24 128 | 36 46",
            "65: 58 24 | 77 36",
            "45: 24 117 | 36 88",
            "91: 24 52 | 36 110",
            "90: 68 36 | 44 24",
            "98: 24 36",
            "106: 24 92",
            "88: 24 103 | 36 6",
            "84: 86 24 | 41 36",
            "78: 24 44 | 36 125",
            "26: 72 24 | 99 36",
            "37: 128 24 | 54 36",
            "130: 24 9 | 36 121",
            "41: 36 32 | 24 81",
            "122: 19 36 | 68 24",
            "1: 24 19 | 36 92",
            "28: 124 36 | 125 24",
            "76: 24 92 | 36 93",
            "77: 55 36 | 131 24",
            "83: 24 28 | 36 73",
            "86: 24 116 | 36 123",
            "2: 104 36 | 33 24",
            "92: 36 24",
            "104: 98 24 | 109 36",
            "32: 24 114 | 36 103",
            "124: 36 36 | 36 24",
            "61: 44 36 | 68 24",
            "35: 36 63 | 24 87",
            "129: 36 69 | 24 18",
            "40: 24 29 | 36 30",
            "31: 10 24 | 118 36",
            "30: 109 24 | 6 36",
            "117: 36 109 | 24 114",
            "89: 36 52 | 24 54",
            "108: 14 36 | 39 24",
            "36: \"a\"",
            "112: 78 24 | 62 36",
            "87: 24 53 | 36 95",
            "51: 100 24 | 30 36",
            "107: 1 24 | 85 36",
            "34: 24 | 36",
            "4: 24 68 | 36 109",
            "12: 93 24 | 92 36",
            "67: 36 93 | 24 109",
            "46: 6 36 | 124 24",
            "80: 24 57 | 36 68",
            "13: 16 36 | 71 24",
            "68: 36 24 | 24 34",
            "42: 20 24 | 35 36",
            "131: 24 74 | 36 111",
            "3: 24 19",
            "118: 70 24 | 22 36",
            "64: 21 24 | 126 36",
            "70: 130 24 | 84 36",
            "72: 102 24 | 49 36",
            "121: 106 24 | 59 36",
            "126: 113 24 | 25 36",
            "",
            //"abbbabaaaabbbaaaaabbabbb",
            //"ababaaabaababbaaabbaabbababbbbbb",
            //"abaaaaaabbbaaaaabbbbbbbababaabab",
            //"bbabababbabaaabbabbababa",
            //"abaabaababaaaabbbabbbabaaaabbbbb",
            //"aabababbbabbbbaaaabbaaba",

            "aabababbbaaaabbababbabba",

            //"bbabababbbabaaaababbbaaa",
            //"abaaabbbaaabbbbabaaaaaabbbbbbabb",
            //"bbbbbaabbabbaabaaabbbaaaabbaabbabbbabbbbababbbba",
            //"aaabababbabaaaabbbbaabababbaaabbbabababaaaaabaaaaaaaabbbabbbaaaaaaabbabbbbbaaabb",
            //"babbbbabaaaabbbabbbbaabb",
            //"babbaaabbbbbaaaababababababbabbaaabbbbaa",
            //"babbbbbabbaabbababaaabba",
            //"abbbbbbbaaabbaabbbaabababbbbaaabababbbabbabbabba",
            //"abbbbaaabaaaabbaaabbaaabbaabbbabbbbabaabbaabbababbbaabba",
            //"aababbabababbabaababbbba",
            //"babaaabbaabbabbabbbbbaaa",
            //"aaaaaababbbababaabbbbababbaaabbabaaaabab",
            //"baaaaaaaaaaabbaaabbbbbabbbbbaaaa",
            //"aabbaabaabaabbbbbbaaaabbbbbbabba",
            //"bbbbbbaabbbbbbbbbbabbaaa",
            //"abaabababbbbbbbabaabaaaaaaabbaabbaaabbbb",
            //"bababaaaaababaaaababaabbbaababbbbaaabaaaaababbbbbbaababbaaaabaaa",
            //"bbbbaaabbbabaaabababaaaaaaaaaabaabbbabba",
            //"bbabbbbbbabaaabaaabaaaabaaababbbbaaabbaa",
            //"aabaaababbbababaabaaabba",
            //"bababaababbbbaabbbbbbaabbbabaaabaabbabaaabaabbab",
            //"babaaaaababbbabbabaaabba",
            //"ababbabaababaabababaabbbbbaaabaababbabba",
            //"abbbbabababbaabaabbabaaababbaaaabbababbb",
            //"aabaaaaabbaaaaabbbbabbabbbbbabab",
            //"abababbaabbbaaababbbabbbbaaabbbbbaaabbbb",
            //"aaaaaababaabaaaaaaabbbbabbbbbbbbbaabbbbabaaabaaa",
            //"aabbbbabaabaaaaabbbaabbb",
            //"ababbabaaabbbbabbbbaaaab",
            //"aabbabaaabbbbbbabaaabbaa",
            //"abaabbbabbbabbababbbaaaaaabaaabababbbbaa",
            //"aabababbbabbbbaabaabbbbb",
            //"bbaaababaabbababbabaabbbbbbbabab",
            //"bbbbabbaaabbaabaaaaabaaaaabbaabbabababbabbbbaaaaaaaabbabbbbaabab",
            //"abaaaaaaabbbbaabbbbabaaa",
            //"ababaaabababaaabbbabbaaa",
            //"bbbbaaababbbabaaabbbabba",
            //"ababababaabababbababababaaaaaabababbabbbbabbbaaaabbaaabb",
            //"aababbaaaaababaabbabbaabbaaabbabaabbbbbbabababaabaaabaaa",
            //"bbbbaaaaaabbbbbbbbbabbabaaaaabbbbaabbaba",
            //"babbbbabbbbbbaabaabbbabb",
            //"aabbbbabababaaaababababa",
            //"bbbbbbbbabbaaaaababababb",
            //"bbabaaabbaabaaabbbbbaabbbaaaabbaaabbbbaaaabbabaabbaabaaaaaaaaaba",
            //"bbaabbaabababbbabababaaa",
            //"aaaabbaaabababaaaabbbaba",
            //"bbbbaaabbbaabbaaaaaabaabaaaaabbbaabbaabbbabababa",
            //"ababbabaabbbbbaaabaababaaabbabbabaaaaabbaaaaabbbbaaabbaa",
            //"baabababbabbaaababaaababbbaaababbbaababb",
            //"aaabbabaabbbaaaabaabaabb",
            //"aaababbbbbabababbabbbbabbbbbbabaaaabbbbbbabbabab",
            //"babaaabbbbbabababaabbaaa",
            //"abbabbababbbbaaaababbbba",
            //"bbaaabaabaaabababbbabbababaabbaaaaaabbaabbbbbaba",
            //"aaababbbababaabbbbaaabbb",
            //"abbaababaaabbbbabaababaaababaaaaabaaabbbaabbaaba",
            //"baaaababbbbbabbababaabbbaaabbaabbbbbabbbaabaabbbbaaaabaabaabbaba",
            //"aabaaaabaabbabababbbabaabbbabbbb",
            //"baaaaaaabbabababaabbbaaabbbbbbaaabbabbaabbaababa",
            //"baaaabbbabbbaaabbbabbbbbabaabbbb",
            //"babaabbabbbbaaabbbbabaabbbabbababbbbabab",
            //"aabbabbaaaaababaabaaabbbabaabbaaabababaa",
            //"bbbbbbbbabbaababaaaaaabaaabbabaababbbbbabbbaaabbbaabbabb",
            //"abababbaabaabaabbabaabab",
            //"bbabaabbbbabaaabbbaaabbb",
            //"abbbbabaabbababbbbbaaaab",
            //"aaabaaaaaabababbababaabaaaaaabbabbbababbaabbbabbaabbbbbbaabbbbab",
            //"abbbbbbaaabaaaabaaabbbab",
            //"ababaababaabbbbabaaababb",
            //"bbbbbbbaaaabaabababbbbabbbabababbabaabaabbaabaab",
            //"abbbbaabbbbbbbbaabbbabaaababbbbbbbaaaaba",
            //"bbbbbbbababbaaababbabaaabbbaaaaaaaabbbbababbabbabaabbabbbbabaaba",
            //"bbabbababbbbbaabbaaaaabb",
            //"bbaaababaabbabbabababaababbbbbbaabaaaabbbaababbbaaaaabba",
            //"aababbabbbbbaaaaaababaaaabaaaaaaabbbabaaaaabaaaa",
            //"aaaaababbbababaaaaaabbbabbaaabba",
            //"bbbbaaaaabbbbbbabbbaabab",
            //"aabaaaaabbabbababbaababbabbaabaabbaaaaabaaaaaabb",
            //"bbbaaaaaabbaabbabbabaaabaaabbbab",
            //"bbabababbbababaaababbaaa",
            //"bbaaabababbabbbbaaaaabaabaaababb",
            //"bababaabaababbabaabbbabb",
            //"aabbababababababbaabaaba",
            //"aaaaaabababaabbbaaabbaaa",
            //"abaababbaabaaaabbbabaaabababaabbbaabbbaababbbbbbaaabbabb",
            //"bbaaabaababaabbbabbaaaabbbaabaaabaabaaab",
            //"abbaaabbaaababaabaabbabb",
            //"abaababaaaabbababbaaaaabbaaabbbababaaaabaaaabbbbaaaababb",
            //"abbbaaaaaabaaabbbaabababbbbbabaabbbababb",
            //"babaaabbbbbbbbbbababaaababaabaabaaababaa",
            //"baaaaaaabbbabbabbabaabab",
            //"abbbababaabbaaabbaaabbaa",
            //"aababaaaabbaababaababaaaaababaaabbbbaabababaaababbbbbaaabaaabaaa",
            //"aaabbbaaaabbbbbbbaaaabbbbbabbbbbabbaababaabaabbaabbaaaba",
            //"babbbabbabaaababbaaaabbabbbabababaaaabbaaabaabab",
            //"baaaabaabbaaababbababbbabbaaaaabaaabbaaa",
            //"ababbabaaaabbbaababbbaab",
            //"abbabaababbabbbbabaabbaaabaabbaaaaaaabbb",
            //"abbabaabbaaabaaabaaabbaabaabaabbbbabbbbaabababbb",
            //"aaaaaabbabaabaabababababbbbaaaab",
            //"aaaabbabbababaabaabaaaaabbbaaaab",
            //"baaaabbaaabbbbbabaabbbabbabaaaaabaabbaab",
            //"aababbbaabaabaababbababbaabbbaab",
            //"bbbabbabaababbaaabbbabaabbabaaaa",
            //"abbaaaabaabaaaababaaabaa",
            //"ababaabbbaabbbabbabaaababbaabbba",
            //"abbbbabbbabaabbbbaaaaaaaabbbabbbbaaaabbbaaababaa",
            //"aabbaaaaaaaaabaabbabaaaabaabaaabababaabbabaababa",
            //"aabababbbaaabaabababbbbaabababbb",
            //"babaabbaababaababbabaaba",
            //"baaaabaabbbbbabaababaabbaabaababbbbaabbb",
            //"bbabaaababaaaababaabbaaa",
            //"bbababaabbbbbbaababaaabaaababaaaabbaaaba",
            //"bbaaabaabababbabbbbbaabbaaabaabb",
            //"abaabaabbabaaaaaabbaaaabbbabbababbbaaaab",
            //"abbabbbabaaaaaababbbbbbaaaabbbab",
            //"bbabababbbbbaaabbbabbbaa",
            //"aabababbbabbaaababbaaaaabaaababbaaaababb",
            //"aabaababbaaabbbbabbbbbbb",
            //"aabababbbabaabbbbabaabab",
            //"ababaaaaaabbabbabbbbaaaabababaaa",
            //"aabbbbbbabbaaaabbaaaabaaababbbbb",
            //"abaaaabaababaabbabbbbbab",
            //"aabbaaabbabbaaababaabaaabbbaabbb",
            //"abbbabababbabaabaaabbbbabbbaaaaababbaaab",
            //"baaabbababbabaaaababbbabbbaaaaba",
            //"baaaabaababbaaaabbbabaabbabaaaab",
            //"bbbbaaaaaaaabbbabbabbabababbabaabbabbabaababbaaaaaababaa",
            //"aaaaaabaabbbbaaabbbbaabb",
            //"abbaaaaaabaaababbbabbbab",
            //"bababbbbaaaabbabbbaabbbb",
            //"ababbabbbabaabaabbbbbaabaabbbaab",
            //"ababbabaabbaabaababbbbabababaabbabbbbbabbaaabbba",
            //"aaaaababbbbbbababaababababbbbbab",
            //"babbbbaaaaaaaabbbabbabba",
            //"aababbababbbaaabbbaababb",
            //"abaabbbaaabbbbbaabababbb",
            //"abbbaaabaabaabaabbabbaab",
            //"aabbbbabbbaaabaaaaabbbbbbaaaaaabababbaabaabbbbabbaaabbaabbabbababbabbbaabbaabbaaaaabaaba",
            //"babaaabbaaababbabbaabbba",
            //"abbbbabbbabbabaaaaabaabb",
            //"aaaaaabababbabaaababbbbb",
            //"abababbaabaababbbababbabababbbabbaabababbbabaaaabbbaabababbbbbbbbbaabbba",
            //"bbbbabbabbbaaaabaaaabbaaaaaabaaabbbabaaa",
            //"bbbbbbababbbbaaababbabaabaaabaaa",
            //"bbabaabbaaaaaabaabbbbbaabbbaaaba",
            //"aabbbbbabaaaabbbbaababaaaaaabbabbbbababaaababbabaaaababb",
            //"aaaaaabbabbbbababaabbbbabbabbababbabaabbabbbbbbb",
            //"abbbbababbbbbbaaaaabbaaa",
            //"bababaabaabababbbbaaaaabbaababba",
            //"abbaaaaaaaabbabbabbbaabaaabbabbabaabbaabababbbbbaababbaababbabba",
            //"aabbbbbbbbbbaaabbbabaaaa",
            //"babaaabbaabaabaabaaabbaa",
            //"abaaababaababbaaaabbbaabaabaaaabaabbbaaabbbbbbbbbbabbbabaabbaabbabbbbabb",
            //"aaaabbabaaaaababbabaaabbbbaabaaa",
            //"ababaababaaaaaabbaabbbbabbabbaab",
            //"abbbaaabbbbbbbbababaaabaaabbabaabbabaabbaabababbaaaabaabbaaabaab",
            //"aaabbbbaabbbbaaabaaabbababbbbabbbaaaaabb",
            //"abbaababbaaaabbbbbaabaaa",
            //"aabababbaabbabbabababaabbabbaaaaaaaababbaaaabbaa",
            //"bbbbaaaaabbabbbbabbbbababbababba",
            //"abbbaaababbaabbbbbbbaaaaabaaaababbbaaaabaaabbbabbabbaaabbbaabbab",
            //"bbaabbababbaaaaabbabaababbabbbba",
            //"aababbbaababaaabaaaababb",
            //"aaaababaaabaaaababbbbaabbaaaaababbabaaba",
            //"bbbbbbaababaaabbaaaabbabbabaabaababababb",
            //"aabaaabbbabaabbbbabbaabb",
            //"bbbabbabaabaaaabaabaaaababbaaaaaaababaaabaababbabbbbbabbbbbabbaababbbbbb",
            //"bbbbaabaabbbbaaabbbbaaba",
            //"bbbbbbababbbabaabaaaaabb",
            //"aabbabbaaaaaababaaababbbbbbbaaababbaabbabbbabaaa",
            //"abaaaaaaaaabbabaaababbababbbaaaaabababbbaaabaaaa",
            //"bbaaaaabbaaaaaaabaabaaaaaaaababaaaaaaabababaaaabbaaabbbaabaabbab",
            //"baaaabbaabbaaaaaababbabbababaabbbbabbabbbaabbabb",
            //"abbaabaaabaaababbaaaabbabaabbabababbbaaa",
            //"aabbbbbaaaabbaababbaabbb",
            //"aaabbbbabaaaabbbbbbbbaabaaababbbbbaababb",
            //"bbaaaaababaabbaaaabaabaaabbbaaabababbaab",
            //"abbbbaabbbbbaaaababababb",
            //"abbbaaababbaabbabbbbabbb",
            //"abbbabbbbbbbaabababaabbaabbaaaba",
            //"babaaaaaabbabbbbaaaaaaaa",
            //"abbbaaabaaababbbabbbbaabbabaabbaaabbaabaaaababaaaaaaabba",
            //"abbbaaaabaaaabbbaaaabaab",
            //"bbbbbbbaaabaaaaaaaabbbab",
            //"baaaaaabbaabababbaaaaaba",
            //"bbaabbababbbababbababaababbaabaababbbaba",
            //"abaaaabaabbabbbbbabbbbabbbbaaaaaaaaaabaa",
            //"babaaababbaabaabbabbabba",
            //"ababbbaabbbbbbabbaaabaaa",
            //"aaaabbbaabbabbabbaabbaba",
            //"abbbababababaababbbababaaabbbaabbbaaaabb",
            //"abaaababbaababaaaababbaabaaaabbaabbbabba",
            //"ababbabbaabbbbabbabbaabaaaabbaab",
            //"bbbbabaabababbbababbbabbaababaaabbbbbbabababbaaaaabbabbbbbaaaaba",
            //"bbbabaabbabbaabbbaabaababaabbbbbaaaaaaabbaaababb",
            //"aaaaaabaaabbababababaaaaaaaaabbabbbaaaba",
            //"bbabaabbbbaabaabbbaabbbbbaaaaabbbbbabaaa",
            //"aabbabababbabbaaaaaaaabbbabbbaaabaababbb",
            //"aababbaabbabababbaaaabaabbababbb",
            //"aabaabaaaaabaabaaaabbbab",
            //"abbabbaaaababbaaaabaabaaababababbabbaabbbaabaaabaababaab",
            //"baaaabbaabbababbaabaabab",
            //"aabaabaababaabbabbbaaababbbaaaab",
            //"abbaabaaaababbaabaababbb",
            //"abaaaababaabbbbaabbabaaabaaabbbbaababbbb",
            //"bbaaabaaaababaaaaaaababaabbbaabb",
            //"bbababaabbbabaabbbbabaaa",
            //"baababababaaabbbbaaaabbbababbbbb",
            //"abbbbabbbabbaaaaabbaabaaababaaabbbaabbbbaaaabaababbaabbb",
            //"bababaababbabbbabaabababaabbaaaa",
            //"abbababbaaabbaabaabbbbbbbabbaaaaabaabbaabbabababbaabbbaa",
            //"abaabbbaaaabbbaabaaaaaaabbbbaabbbaaaababbbaaabbb",
            //"aaababbabaaaaaaabaabaaba",
            //"aabbabbabababbbaaaabbbbaababababbaabbaabbbaaabba",
            //"aabbabaababbbbaabbabbbba",
            //"babbaaaabbbabababaaababa",
            //"abaaabbbaabaabababababbbababbbbaaaaaaaab",
            //"abbabbabbabbbabbbabababb",
            //"bbaaaaabbabaaabababbbaaa",
            //"bbaaababaaabbbbaabbbaaaaaabaabaabaaaaabbbaabbaaa",
            //"babbabbabbaaabaaabbbabababaaabba",
            //"aaaaababaabbababaaaaabba",
            //"bbaabbabaaaabbababbbbbbb",
            //"abaabaabbababaabbaaaabab",
            //"abaaaabaaaabbbaabbbbaaabababbabaabababbbaabbaaba",
            //"baabbbbabbbaaaaaabbbbababbabababbabaaaaaaaaaabbabbaaaabb",
            //"aaabbabaaabbabbabbabbaab",
            //"bbaaabbbbbabababaaaabaaaababbabbbabaaabaaaababaaaaabbbaa",
            //"abbbbabaabbaaaabbbbababb",
            //"abbbbabbbbabaaabbaabbbabbaababaabbabbbaa",
            //"babbabaaaabababbbbbbbababaabaaaabbabbaab",
            //"aabaaabaaabbabbaabaabaabaaaababb",
            //"babbabaaaaaaababbaababbaaabbaabbabbbaabbbbaabbaaaaabbbaa",
            //"aabbbbbbbbabaaabbbbbbaabaabbaabbbbbabaaa",
            //"aabbbbbbaabbbbbababaaaaababababbaabbaaaa",
            //"babaabbaababbabbbbaaaaba",
            //"aabaabaabbabaababaaaaabbaabababa",
            //"aabbbaaaabbaabaaaabaabababbbbbbaaabbaaabbbabaabbbbaabbbbbabbbaabbbabbabbaaabaaba",
            //"aabbaaabbbaaaabababaabaaaabbaabbabbababbabbbbbaabbbbbbaa",
            //"abbbbaaabbaabbaaabaabbaabbbababbbbaabbba",
            //"bbaabbbbaaabbbabbbbbbbabbaaabbbbaabbbaaaaababbaabbbbaabb",
            //"abbbaaabbababbbaaabbaaba",
            //"babbbbabababaabaaabaaabaaababbaababaaaabbabbbaab",
            //"bbbabbabbabaabbaabbbaaabaabbaaabaaaabbbbbaaababb",
            //"bbaabbabbabbbbaaabbabaaaabababaa",
            //"abbaababaaaaababbbaaaaba",
            //"baaaabbabbabbbbbaaabaabaabbabbbaabaaaabb",
            //"babaaabbbaaaaaabbaaaaabb",
            //"abbbbbbaabbabbbbaabaaabbabbababa",
            //"aaababbbaaaaabababbaaabb",
            //"aababbabaabaaaabbabbbbab",
            //"bbbbbabaabbbabababbababbaaaaaabbaabbbbaa",
            //"aaabbaababbabaaabbaabbaababaaaab",
            //"aabbabaabbbababaabbbbababbbabbab",
            //"abaabbaabaaaabbbbbaabbaaabbaaaabaabbaabb",
            //"baabaaaaabbabaaabbbaaabababbabab",
            //"baaaabbbbbbabaabbaaababa",
            //"abbabbababbbbbaaabaabbba",
            //"abbabbaaabaaaaabaabbbabbbbaaaaba",
            //"aabaabaaababaaaaabbbaabb",
            //"abbabbbabaaabbabbaabbbbb",
            //"aabbababaabaaabbaaababaa",
            //"bbabbababababbbaabbaaaaabababbbabbabaaaaababbbba",
            //"aaabbabaabaabbaabaabbbbb",
            //"bbbbbaabaaabbbaabbbbabaabbabaabbbbabbbaaabbaabbbbbaabbbb",
            //"abbabaabbbabaabbababbababbbbbabb",
            //"abaaaaaaababaabbaabbabbaaabbabbabbabbbaa",
            //"aaabbbbbaaaaabbbbbaaaababbaaabbabbababababbaaabaabbbabbbaabbbbaabbbaaababbbaaabbababaaab",
            //"aababbbbbbbaabaaaaabbbbababbabaabbbbabbaababbaabbbbbabba",
            //"abbabbaababaaaaaabaaaaab",
            //"abaabaabaabbababaabbaababaaababb",
            //"aaabbbbabaaabbabbaabababababaaabbbbbabbaaaaabaabbaaaaabb",
            //"abbbaaaabbaaaaababbabbabbbabaabbbbbaaabbaabbaaaa",
            //"abbababbaabbabaababbbabbbababaabaaabaabb",
            //"aababbaababbbabbaaaababb",
            //"abaaabbbbabaabbbabaabbbaaababbababbaabbbbbabaaaa",
            //"aaabbaabaabbbbabaabbabbaaaaabbbb",
            //"bbbbbbbbaaaaaabbbaaababb",
            //"aabbbaaabaabababaabbbbaa",
            //"aaaaaabbbbbbabaaaaabbababbaabbab",
            //"aabaaaabbabaabbabbaabaaa",
            //"bababbbbaabbaabbbbbbbabbbbaabbbbabbbaababaabaaababbbabababaabaaaaaabbaaabbaababa",
            //"baaaabbbbabbaaaaaabbbaab",
            //"bbbabbabaaaabbbaaaabbabb",
            //"abbbaaabbbbbbbabbabaabaaaaaabaabbaaaabab",
            //"bbbaabbaaabaabbbbbabbaab",
            //"bbabaabbbababbbabbaaaabb",
            //"aaaabbbaaaabbaabbbbbaaaaaababbabaaabbabb",
            //"abbaabaabaabbbbabababbbbaaabaabaabbaabbbbbbaaaabbbaaaaba",
            //"aabbbbbbaabaaaaaababbbbabbabaababababaab",
            //"babaaabbbabbaaabaababaaaaabbbbbbabaabbabaaaababbaababaab",
            //"bbbbaaababbbbbbaabaaaababbbbaaaababbbabaaaabaaaaaababbbb",
            //"bbbbbbbbaaabbbaaaaabbbab",
            //"aaaaababbaabbbababaaaabbabbaabbb",
            //"bbbbbbbbaaabbbaabaaabaaa",
            //"ababbabbabbabbaaabbbbabbabbbabbbbabbbbbb",
            //"baaaabbaaabbbbbbbbabaaba",
            //"bbbbbaababaabbbaabbbabba",
            //"aaabbbaabbabaaabbabbaabababaababaaaaaaabbaaaaababbaabbbb",
            //"abbaaaabababaaabbbbbbbbbbabaabab",
            //"bababbabaaaaaababbbabbaa",
            //"ababaabbbabaaabbbbbaabab",
            //"abaabaabbbbbbbbbbabbabaaaababaab",
            //"ababaabaaababbabbaaaaaba",
            //"abbaaaaaabaababbaababbabaaabbaabaabaabbbbbbaabaa",
            //"baaaabbbbbbbabaaabbbbababbaabbba",
            //"aabaaabaabaabaabaabbabbabbbbaabaabbbbabbbbaababb",
            //"bbbbbbababbbabaaabaaaaaaaaabbaabababaaaababbabbabaaabaaa",
            //"bbabbabaabaabaababbbabbbaabbbaaabaaaabbbbbaaaaaaabbbbbbb",
            //"babaabbaabbaaaaaaabbaaabbbbaaaaabaaababbbaabbaaa",
            //"abbbababbbaabbaaaaaabaaa",
            //"abaabaaababaaabbabaabaabbabaabbaabaabbbb",
            //"aabababbaaaabbbaababbbbb",
            //"abbaabababaaaabbbaaabbba",
            //"aaaababaabaabbaabaababaababbbaaa",
            //"abbbbaaabaaaabbbaabbbbabaabaaabbabbbbbbabbbabbba",
            //"aabbbbbbbabbabaabaabaaaababaaababbbabbba",
            //"baabbbbaababaaaaaabaaabbabbbaaba",
            //"abaaaabbabbaababbabbaaabbabbbbbb",
            //"bbbbaaaababbaabaaabbbbba",
            //"abbaabbaaaabbababaaaaaaaabbabbbaaabbbaababaaabbaaaababab",
            //"aaabaaaaabbbbbbbbabaabaaaaababaaaabbbabb",
            //"bbaaaaabaaabbbaaaaaaaaab",
            //"aabbbbbbaaabbaabbaaabaab",
            //"abbbbbbbbbbaabbbbabaabbbbabbababaababaaaaaaaaaaaaaabaaaa",
            //"aababbbaabaaabbbbaaaaaababababbb",
            //"baabababaabbbbbaaaaaaabbbaaabbaaababbaaaaaabbbbbbabbabababaabbabbbaaabba",
            //"babaaaaabbbbaabaabbbabbbbabababa",
            //"aaababbaababaabaabbbaaabbabababa",
            //"bbababaaabaaabbbbaaaabaaabbbbaababbababababbabbabbaaabbb",
            //"abbbababbabaabbbaaaabbbababaabaa",
            //"bbabbaaaaaabababaaababab",
            //"abaaaabbabbabaaaaabaabaaaaabbbbaabbabbaaabbbbbab",
            //"babbaaaaabaabababaaaabab",
            //"ababaaabbaabbbbbbaababbabbaabbababbabbbbbababbaababbbbba",
            //"aabbbbbabaaabbabababbabbbaaaaabb",
            //"abaaaaaabbaaaaabbabbaabb",
            //"abbabaababaabababababbbaabababbb",
            //"baaaabaaabbabbbaabbbaaabbaaabaaa",
            //"abbbbbabbbaaaabbabbbaabaaaabaaaabbabaaaaababaaba",
            //"aabbbbabbbaaabaabbababbabbbbaabaaaaababbbabbbbabbabbaabb",
            //"bbababaaababbbaaaabaabaabbbbabaaaabbbabb",
            //"abbbbbaaaabaabaababaabab",
            //"abbbaaaaabbaabbabaabbbaa",
            //"aaaaabababababbaaabaaaaaaabaaababbabaaabbababbaaabbbbbabaabbaaaa",
            //"abaabaabbbaaaaabaabbabaaaaabbbbbababaaaaabaabbbabaaabababbbaabaaaabbbbaaaababaababaabbaa",
            //"babbbbaabbabbabbbbbbbaaabaaababa",
            //"ababbabbbbbabbabbaabaaba",
            //"bbbabaababbaaaaaabaabaaaabaaabbbbbbaabba",
            //"bbbbbbbbbabaabbababaabaa",
            //"aaaabababababbababbaaabb",
            //"ababaabbbabaaababaababba",
            //"aababbaaababaabbbabaaaab",
            //"babbaababbbbaaabbbbababa",
            //"ababbabababbbbbaabbbabaabababbbbbbbaaabb",
            //"baabbbabaaabbaabbbbabbbb",
            //"aabbbbaaaaaaaabbabbaaaaababbbabbbababbbbabaabbba",
            //"baaaabaaaaaaaababaabbbbabbbabbbabababbaa",
            //"aababbaaaaaabababbaaaaabbbabbabaabbaaabbaaaabaab",
            //"ababaabaabaaabbbababbaab",
            //"bbaabbaaaaaababbabbaaabbaabbababababaabaabbaabaaaaabababaabbabbb",
            //"bbaaababbbabababbbbbaaaabbaabbbb",
            //"bababaabbabaaababbaabaab",
            //"baaaaaaababbbbbabbbbbabb",
            //"aababbabbbbbbbababababbb",
            //"bbbbbbababaaaabbbbaaaabb",
            //"babaaaaaababaabbbabaababbbabbbaabaaabbaababaabaababbbaba",
            //"abbbabababbbbbbababaabab",
            //"aabaabaabaaaabbabbaabbabbbabbaaaababbbbb",
            //"abaababbababaababaababba",
            //"abaaaabbbaababbabbbababbabbabababbbbbbbbaabbaaababbbababbbbabbaabaabbabaabbabbbb",
            //"abaaabbbbaabababaaaaaabababbbbbabbbababaabbbaaaababaabab",
            //"bbaabbaababaaabaaabaaabb",
            //"abbabaabbabaaaaaabbbbbbaaabbababbbbaaaab",
            //"aabbbbaaabbaababaaaaaabaaaababbabababbaabbaaaabaaaaababbabbbbbabbbabaabaababbbabbabbabaa",
            //"ababaaaaababaabbbababbbbbabbabaabbbabbabababbaabbaabaaabbbbbaabbbaabbbbbaaaaabaabbbbbabb",
            //"abbbbababbbbaaabaabbbbabaaabbaaabbbbbaaa",
            //"babbbbaababbbbbabbbbbabaabbababbababbaab",
            //"abbbbabbaababbaabbbbbbaaaaabaaaaaabbbbaa",
            //"aaababbbbaaaaabaaaabbbbaaabbbbaaabbaaaaaabbbbbbabbbbbbaabababbba",
            //"ababaaababaabababbbaaaaabbaaabbb",
            //"abaabbaabaaabbbbbbabbaabbbaaabba",
            //"ababaabbbaabaaaaababaaababababaabbabbbba",
            //"bbaaababaaaabbababbbbabaababababbbbaaabb",
            //"aababbaaaabbbbabbabaaababbbbabba",
            //"bbabaabbaababbbababaabbaabbbbbaababbbaba",
            //"aaabbaabbaabaaaaabbaaaba",
            //"abbaabbabaabababbbaaaaba",
            //"aaaabbabaababaaaabaaaabaaabbbbbbbabbabaabbbbbbabbabbbaaaabbaaaba",
            //"aaaababaabaabababaabaaab",
            //"babaaabbabaababaabaabbaaaaababbbbbbaaababaabbaab",
            //"aababbaabbbabbabbaaababa",
            //"bbbbbbbaabaabbbabababaabbabbbaabbbaabaab",
            //"bababbabbbbbaaabbaabaaba",
            //"aaaaabababbabaaaaaaaaaab",
            //"babbbbabaabbbababbbbaabaabaabbaaaaabaabaaaaababbbabaaabaababbaaabaaabbaaaabbbbaaaaabbbabbbaaaaba",
            //"aabbababaabaaaabbabbbbaabbbbbbabaabaabbaaaababaa",
            //"ababbabbabaaaabaaaabaaab",
            //"abbabbbbabaabbbaababbababbbbabbbaaabbabb",
            //"ababaaaabaabaababaaabbbaaabababbabbbabaaaabaaaaaaaaaabbb",
            //"baabbbabaaaababbaaaabbbbbbbbabba",
            //"bbabababbaaaabbbbabbaaabaabbaaba",
            //"babbbbabbbababaabbaaabbb",
            //"abbbabaaabbaaaabbbbbbbabababbbabababbaab",
            //"bbaaaaabbaaaaaabbbabbbaa",
            //"abbabbababbbbbbaabbabbabbabbbabbabaabaaaaaabbbabbbabaaba",
            //"ababaaaaaaaabaababbabaababaaabbbababbabbababababbbaaaaaabaababba",
            //"bbababaabaaaabaaabaaababababbbabaaabbaabbaaaaaabbaabbaaa",
            //"abbbaaabaaababbabaabaaaabaaabaaa",
            //"abbaaaabbaaaabbbbbbbbabaabbaaaaaabbabaabbbaaabbbbbaababa",
            //"bbaaaabaaabaababaabbaabbbbabbbbabbaabaab",
            //"baababaaabbbbbaaaabaaaabaaababaa",
            //"bbabbababbbbbbbbaaabbbab",
            //"abaababbabababaabbbabaaaabbaaababaaabaab",
            //"bbabbbbababaaaababbbaabbabbbaabb",
            //"bbbaaaaaabaaaabbbbabbabb",
            //"aaaaababaaaabbabbbbbbbabaabbababbbbaaaab",
            //"ababbbaabaababaabaaabaab",
            //"aabaaaaababbbbaaabbbaabb",
            //"bbbbbbbbaaabbbbbaaaabbbabaababbbabbaababaaaaaababbbaaabbbabbabbbbabaaaab",
            //"bbbbbbbbaabaaaababaaabababbbaaba",
            //"baabaaabaaababaaabbbbababbbababaaababbbaaaaabbbbaabbaaab",
            //"bababaababbababbbbbbbabababaabbbbaabbabbaaaababb",
            //"bbbabbababaabaaaaaaababaaabaabbabaabbbbb",
            //"ababbbabbbbbaababbaabbaababaaaaabaaabababbabaaaa",
            //"abbabbbbbaaaaaaaaaabbabb",
            //"abababbaababaaaaababbbaaabaabbaabbaaaaba",
            //"bbaaaaababbaabaabbbaaaba",
            //"baababaaaabaaabbbbaabaab",
            //"babbbbabaaaabbbaabbaabaabbabababaaabbbbb",
            //"babbbbaaabbbababaabbaaabbabbaabb",
            //"bbabaabbabaabaaabbabbbba",
            //"babbabaaabbbbbaaaabbbabb",
            //"aababbbabbaabbabababbbbb",
            //"aabaaaabbaabababaabababbabbbbaababbbbbaabaaabaaa",
            //"aabbbaaaabbabbbaabaaababbababbbababababababaaaab",
            //"abbbbbaabbaaabababbbabbb",
            //"abbaaaaaabaabbaabbbbaababaaabbbbbbbbbababbbbbbaabaaaaabababbbbbbbbaabbaaaaaaaaabbabaaaab",
            //"bbaaabaabbaaaaabaabbaabb",
            //"aababbbabaaabbabbbbaabba",
            //"abaabababbbbaaaabababbbabbbababaabbbababbabbaaabaaabbbab",
            //"baaaabbabbbbbaabbaababaaaaaaaabaaabbbbaabbbaaaabaaababab",
            //"abbababbbbbbaababaabbbababbaabbaabababbabbabaaaababbabbb",
            //"bbabbbbbbabbabaaabababbb",
            //"aaaabbabbabaaaaaaaaaababbabbabbb",
            //"aabaaaabbbabaabbababaabaaaaaaaaa",
            //"bbbbbaababbbabbbabbaaaabbbbbbbbaaabaabaa",
            //"aaaabbbaabbabbbbabbaaaaababbabaabbbbabaabbbabbaabbbbaabb",
            //"baaaaaaabbbaaaaaaaababbbbbaaabbbabaabbbb",
            //"aabbbbbbbaaabbababbaaaba",
            //"bbabaababbbababbaaabbaaa",
            //"bbbbabbaaabbbbaaaaaaabbaaaabbbaaabaababaabababbbbbabbbabbbbbabbb",
            //"abbaaaabaabbbaaabbbbabaaaaaabaabbabbaabb",
        };

    }
}

