using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day23
    {

        public string Solve1(string cupsString, int moves)
        {
            var cups = new LinkedList<int>(
                cupsString
                    .ToCharArray()
                    .Select(c => (int)(c - '0'))
                    .ToArray());

            var currentCup = cups.First;

            // circular linked list helper methods
            LinkedListNode<int> GetNextCup(LinkedListNode<int> cup) => cup.Next == null ? cups.First : cup.Next;

#nullable enable
            LinkedListNode<int>? FindCup(LinkedListNode<int> fromCup, int value)
            {
                if (fromCup.Value == value)
                    return fromCup;

                var cup = GetNextCup(fromCup);
                while (cup != fromCup)
                {
                    if (cup.Value == value)
                        return cup;

                    cup = GetNextCup(cup);
                }

                return null;
            }
#nullable disable


            var threeCups = new LinkedListNode<int>[3];

            for (int i = 0; i < moves; i++)
            {
                // get and remove three next cups
                threeCups[0] = GetNextCup(currentCup);
                threeCups[1] = GetNextCup(threeCups[0]);
                threeCups[2] = GetNextCup(threeCups[1]);
                for (int j = 0; j < threeCups.Length; j++)
                    cups.Remove(threeCups[j]);

                // select destination cup
                var destinationCupValue = currentCup.Value - 1;
                var destinationCup = FindCup(currentCup, destinationCupValue);
                while (destinationCup == null || destinationCup == currentCup)
                {
                    destinationCupValue -= 1;
                    if (destinationCupValue < 1)
                        destinationCupValue = 9;

                    destinationCup = FindCup(currentCup, destinationCupValue);
                }

                // place removed three cups after destination cup
                for (int j = 0; j < threeCups.Length; j++)
                {
                    cups.AddAfter(destinationCup, threeCups[j]);
                    destinationCup = threeCups[j];
                }

                // select new current cup
                currentCup = GetNextCup(currentCup);
            }

            // create output string
            return string.Create(8, cups.Find(1), (chars, startCup) =>
            {
                var cup = GetNextCup(startCup);
                for (int i = 0; i < 8; i++)
                {
                    chars[i] = (char)('0' + cup.Value);
                    cup = GetNextCup(cup);
                }
            });
        }


        public UInt64 Solve2(string cupsString, UInt64 moves)
        {
            var maxValue = 1000000ul;
            var cups = new LinkedList<UInt64>(
                cupsString
                    .ToCharArray()
                    .Select(c => (UInt64)(int)(c - '0'))
                    .ToArray());

            var currentCup = cups.First;

            var cupsMap = new Dictionary<UInt64, LinkedListNode<UInt64>>((int)maxValue);

            // add first cups to the map
            var tmpCup = cups.First;
            while (tmpCup != null)
            {
                cupsMap.Add(tmpCup.Value, tmpCup);
                tmpCup = tmpCup.Next;
            }

            // add rest of the cups to the list and map
            for (UInt64 i = 10ul; i <= maxValue; i++)
            {
                cups.AddAfter(cups.Last, i);
                cupsMap.Add(i, cups.Last);
            }

            // circular linked list helper methods
            LinkedListNode<UInt64> GetNextCup(LinkedListNode<UInt64> cup) => cup.Next == null ? cups.First : cup.Next;
            LinkedListNode<UInt64> FindCup(UInt64 value) => cupsMap[value];


            var threeCups = new LinkedListNode<UInt64>[3];

            for (UInt64 i = 0; i < moves; i++)
            {

                // get and remove three next cups
                threeCups[0] = GetNextCup(currentCup);
                threeCups[1] = GetNextCup(threeCups[0]);
                threeCups[2] = GetNextCup(threeCups[1]);
                cups.Remove(threeCups[0]);
                cups.Remove(threeCups[1]);
                cups.Remove(threeCups[2]);

                // select destination cup
                var destinationCupValue = currentCup.Value - 1;
                if (destinationCupValue < 1ul)
                    destinationCupValue = maxValue;
                var destinationCup = FindCup(destinationCupValue);
                while (destinationCup == currentCup ||
                       threeCups[0] == destinationCup ||
                       threeCups[1] == destinationCup ||
                       threeCups[2] == destinationCup)
                {
                    destinationCupValue -= 1ul;
                    if (destinationCupValue < 1ul)
                        destinationCupValue = maxValue;

                    destinationCup = FindCup(destinationCupValue);
                }

                // place removed three cups after destination cup
                for (int j = 0; j < threeCups.Length; j++)
                {
                    cups.AddAfter(destinationCup, threeCups[j]);
                    destinationCup = threeCups[j];
                }

                // select new current cup
                currentCup = GetNextCup(currentCup);
            }

            // calculate output
            var oneCup      = FindCup(1ul);
            var firstAfter  = GetNextCup(oneCup);
            var secondAfter = GetNextCup(firstAfter);

            return firstAfter.Value * secondAfter.Value;
        }


        public static readonly string PUZZLE_INPUT = "156794823";

    }
}

