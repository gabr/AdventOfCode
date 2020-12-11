using System;
using System.Linq;
using System.Collections.Generic;

namespace Solutions
{
    public class Day10
    {
        public int Solve1(int[] adapters)
        {
            Array.Sort(adapters);

            int numberOf1Diffs = 0;
            int numberOf3Diffs = 0;

            if (adapters[0] == 1) numberOf1Diffs += 1;
            if (adapters[0] == 3) numberOf3Diffs += 1;

            for (int i = 0; i < adapters.Length - 1; i++)
            {
                int diff = adapters[i + 1] - adapters[i];

                if (diff == 1) numberOf1Diffs += 1;
                if (diff == 3) numberOf3Diffs += 1;
            }

            // for built-in
            numberOf3Diffs += 1;

            return numberOf1Diffs * numberOf3Diffs;
        }

        public UInt64 Solve2(int[] adapters)
        {
            Array.Sort(adapters);

            var buffer = new int[adapters.Length + 2];
            buffer[0] = 0;
            buffer[buffer.Length - 1] = adapters[adapters.Length - 1] + 3;
            Array.Copy(adapters, 0, buffer, 1, adapters.Length);

            UInt64 result = 1;
            int onesCount = 0;
            int tmp       = 0;

            for (int i = 1; i < buffer.Length; i++)
            {
                if (buffer[i] - buffer[i - 1] == 1)
                {
                    onesCount += 1;
                }
                else
                {
                    for (int j = 1; j < onesCount; j++)
                        tmp += j;

                    result *= (UInt64)(tmp + 1);
                    onesCount = tmp = 0;
                }
            }

            return result;
        }

        public static readonly int[] PUZZLE_INPUT =
        {
            48,
            171,
            156,
            51,
            26,
            6,
            80,
            62,
            65,
            82,
            130,
            97,
            49,
            31,
            142,
            83,
            75,
            20,
            154,
            119,
            56,
            114,
            92,
            33,
            140,
            74,
            118,
            1,
            96,
            44,
            128,
            134,
            121,
            64,
            158,
            27,
            17,
            101,
            59,
            12,
            89,
            88,
            145,
            167,
            11,
            3,
            39,
            43,
            105,
            16,
            170,
            63,
            111,
            2,
            108,
            21,
            146,
            77,
            45,
            52,
            32,
            127,
            147,
            76,
            58,
            37,
            86,
            129,
            57,
            133,
            120,
            163,
            138,
            161,
            139,
            71,
            9,
            141,
            168,
            164,
            124,
            157,
            95,
            25,
            38,
            69,
            87,
            155,
            135,
            15,
            102,
            70,
            34,
            42,
            24,
            50,
            68,
            169,
            10,
            55,
            117,
            30,
            81,
            151,
            100,
            162,
            148,
        };

    }
}

