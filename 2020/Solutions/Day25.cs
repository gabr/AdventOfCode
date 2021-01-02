using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day25
    {
        public UInt64 TransformKey(UInt64 key, UInt64 subjectNumber, int times)
        {
            for (int i = 0; i < times; i++)
            {
                key *= subjectNumber;
                key %= 20201227ul;
            }

            return key;
        }

        public int DetermineLoopSize(UInt64 targetKey)
        {
            var subjectNumber = 7ul;
            var key = 1ul;

            for (int i = 0; i < int.MaxValue; i++)
            {
                if (key == targetKey)
                    return i;

                key = TransformKey(key, subjectNumber, 1);
            }

            return -1;
        }

        public UInt64 Solve1(UInt64[] publicKeys)
        {
            //var cardsLoopSize = DetermineLoopSize(publicKeys[0]);
            var doorsLoopSize = DetermineLoopSize(publicKeys[1]);

            return TransformKey(1ul, publicKeys[0], doorsLoopSize);
        }


        public static readonly UInt64[] PUZZLE_INPUT =
        {
            5099500ul,
            7648211ul,
        };

    }
}

