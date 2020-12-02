using System;

namespace Solutions
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine((new Day01()).Solve1(Day01.PUZZLE_INPUT));
            Console.WriteLine((new Day01()).Solve2(Day01.PUZZLE_INPUT));

            Console.WriteLine((new Day02()).Solve1(Day02.PUZZLE_INPUT));
            Console.WriteLine((new Day02()).Solve2(Day02.PUZZLE_INPUT));
        }
    }
}
