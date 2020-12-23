using System;
using System.Linq;
using System.Collections.Generic;

namespace Solutions
{
    public class Day15
    {

        public UInt64 Solve(int[] startingNumbers, UInt64 turns)
        {
            var numbers = new Dictionary<UInt64, UInt64>();

            UInt64 i = 0;
            UInt64 previous = 0;
            bool wasFirstTime = true;

            for (; i < (UInt64)startingNumbers.Length; i++)
            {
                numbers[(UInt64)startingNumbers[i]] = i;
                previous = (UInt64)startingNumbers[i];
            }

            for (; i < turns; i++)
            {
                UInt64 current = 0;
                if (wasFirstTime == false)
                    current = (i-1ul) - numbers[previous];
                numbers[previous] = i - 1ul;

                wasFirstTime = numbers.ContainsKey(current) == false;
                previous = current;
            }

            return previous;
        }

        public static readonly int[] PUZZLE_INPUT =
        {
            1,0,16,5,17,4
        };

    }
}

