using System;
using System.Linq;
using System.Collections.Generic;

namespace Solutions
{
    public class Day13
    {

        public int Solve1(string[] notes)
        {
            int timestamp = int.Parse(notes[0]);

            int[] busesIds = notes[1].Split(',')
                .Where(id => id != "x" && id != "")
                .Select(id => int.Parse(id))
                .ToArray();

            var minutes = new int[busesIds.Length];
            int theSmallestMinutesIndex = 0;

            for (int i = 0; i < busesIds.Length; i++)
            {
                int id = busesIds[i];
                var rest = timestamp % id;
                if (rest != 0)
                    rest = id - rest;
                minutes[i] = rest;

                if (minutes[i] < minutes[theSmallestMinutesIndex])
                    theSmallestMinutesIndex = i;
            }

            return minutes[theSmallestMinutesIndex] * busesIds[theSmallestMinutesIndex];
        }

        public struct Bus
        {
            public UInt64 Id;
            public UInt64 ExpectedMinutes;
        }

        public UInt64 Solve2(string[] notes)
        {
            int comasCount = 0;
            int xesCount   = 0;

            var ids = notes[1].AsSpan();

            for (int i = 0; i < ids.Length; i++)
                     if (ids[i] == ',') comasCount += 1;
                else if (ids[i] == 'x') xesCount   += 1;

            int numbersCount = comasCount - xesCount;
            var buses = new Bus[numbersCount];

            int busIndex       = 0;
            int lastCommaIndex = -1;
            comasCount         = 0;

            UInt64 smallestBusId = UInt64.MaxValue;
            UInt64 biggestBusId  = 0;

            UInt64 allBussesIdsProduct = 1;
            UInt64 allBussesIdsSmallProduct = 1;

            for (int i = 1; i < ids.Length; i++)
            {
                // got a buss id, process it
                if (ids[i] == ',' && ids[i - 1] != 'x')
                {
                    var id = UInt64.Parse(ids.Slice(lastCommaIndex + 1, i - (lastCommaIndex + 1)));

                    buses[busIndex].Id = id;
                    buses[busIndex].ExpectedMinutes = (UInt64)comasCount;

                    allBussesIdsProduct *= id;
                    allBussesIdsSmallProduct *= (id - buses[busIndex].ExpectedMinutes);

                    if (id > biggestBusId)  biggestBusId = id;
                    if (id < smallestBusId) smallestBusId = id;

                    busIndex += 1;
                }

                // count and skip 'x'
                if (ids[i] == ',')
                {
                    comasCount += 1;
                    lastCommaIndex = i;
                }
            }

            return FindMinX(
                buses.Select(b => b.Id).ToArray(),
                buses.Select(b => b.ExpectedMinutes == 0 ? 0 : b.Id - b.ExpectedMinutes).ToArray(),
                (UInt64)buses.Length);
        }

        // Returns modulo inverse of
        // 'a' with respect to 'm'
        // using extended Euclid Algorithm.
        // Refer below post for details:
        // https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/
        public static Int64 inv(Int64 a, Int64 m)
        {
            Int64 m0 = m, t, q;
            Int64 x0 = 0, x1 = 1;

            if (m == 1)
            return 0;

            // Apply extended
            // Euclid Algorithm
            while (a > 1)
            {
                // q is quotient
                q = a / m;

                t = m;

                // m is remainder now,
                // process same as
                // euclid's algo
                m = a % m; a = t;

                t = x0;

                x0 = x1 - q * x0;

                x1 = t;
            }

            // Make x1 positive
            if (x1 < 0)
            x1 += m0;

            return x1;
        }

        // k is size of num[] and rem[].
        // Returns the smallest number
        // x such that:
        // x % num[0] = rem[0],
        // x % num[1] = rem[1],
        // ..................
        // x % num[k-2] = rem[k-1]
        // Assumption: Numbers in num[]
        // are pairwise coprime (gcd for every pair is 1)
        static UInt64 FindMinX(UInt64 []num, UInt64 []rem, UInt64 k)
        {
            // Compute product
            // of all numbers
            UInt64 prod = 1;
            for (UInt64 i = 0; i < k; i++)
                prod *= num[i];

            // Initialize result
            UInt64 result = 0;

            // Apply above formula
            for (UInt64 i = 0; i < k; i++)
            {
                UInt64 pp = prod / num[i];
                result += rem[i] *
                          (UInt64)inv((Int64)pp, (Int64)num[i]) * pp;
            }

            return result % prod;
        }

        public static readonly string[] PUZZLE_INPUT =
        {
            "1000066",
            "13,x,x,41,x,x,x,37,x,x,x,x,x,659,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,29,x,409,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,",
        };

    }
}

