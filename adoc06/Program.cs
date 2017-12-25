using System;
using System.Linq;
using System.Collections.Generic;

namespace adoc06
{
  class Program
  {
    static void Main(string[] args)
    {
      int[] test = new int[] { 0, 2, 7, 0 };
      //Console.WriteLine(solve1(test));

      int[] data = new int[] { 5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6 };
      //Console.WriteLine(solve1(data));

      var s = solve2(data);
      Console.WriteLine($"{s.redistributions} {s.cycleFromLast}");
    }

    static int indexOfMax(int[] input)
    {
      if (input.Length == 0) { return -1; }

      int maxIndex = 0;
      int max = input[0];

      for (int i = 1; i < input.Length; i++)
      {
        if (input[i] > max)
        {
          maxIndex = i;
          max = input[i];
        }
      }

      return maxIndex;
    }

    static void redistribute(ref int[] input, int maxIndex)
    {
      int toRedistribute = input[maxIndex];
      input[maxIndex] = 0;
      int index = maxIndex + 1;

      while (toRedistribute > 0)
      {
        if (index >= input.Length) { index -= input.Length; }

        input[index] += 1;
        index += 1;
        toRedistribute -= 1;
      }
    }

    static int solve1(int[] input)
    {
      HashSet<string> previousStates = new HashSet<string>();

      int maxIndex = 0;
      int redistributionsCount = 0;

      string currentState = string.Join(",", input);

      while (false == previousStates.Contains(currentState))
      {
        //Console.WriteLine(currentState);
        previousStates.Add(currentState);
        maxIndex = indexOfMax(input);
        redistribute(ref input, maxIndex);
        currentState = string.Join(",", input);
        redistributionsCount += 1;
      }

      return redistributionsCount;
    }

    static (int redistributions, int cycleFromLast) solve2(int[] input)
    {
      Dictionary<string, int> previousStates = new Dictionary<string, int>();

      int maxIndex = 0;
      int redistributionsCount = 0;

      string currentState = string.Join(",", input);

      while (false == previousStates.ContainsKey(currentState))
      {
        //Console.WriteLine(currentState);
        previousStates.Add(currentState, redistributionsCount);
        maxIndex = indexOfMax(input);
        redistribute(ref input, maxIndex);
        currentState = string.Join(",", input);
        redistributionsCount += 1;
      }

      return (redistributionsCount, redistributionsCount - previousStates[currentState]);
    }
  }
}
