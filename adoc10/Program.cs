using System;
using System.Linq;

namespace adoc10
{
  class Program
  {
    static void Main(string[] args)
    {
      //string test_input1 = "3, 4, 1, 5";
      //Console.WriteLine(Solve1(test_input1, 5));

      //string input = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108";
      //Console.WriteLine(Solve1(input));

      //(string test, string expectedResult)[] tests2 = new []
      //{
      //  ("", "a2582a3a0e66e6e86e3812dcb672a272"),
      //  ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
      //  ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
      //  ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
      //};

      //foreach (var t in tests2)
      //{
      //  Console.WriteLine("Got:      " + Solve2(t.test));
      //  Console.WriteLine("Expected: " + t.expectedResult);
      //  Console.WriteLine();
      //}

      string input = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108";
      Console.WriteLine(Solve2(input));
    }

    public static int[] Hash(int[] data, int hashSize, int rounds = 1)
    {
      Console.WriteLine("Input bytes: " + string.Join(", ", data));

      int[] hash = new int[hashSize];
      for (int i = 0; i < hash.Length; i++)
        hash[i] = i;

      int tmp = 0;
      int a, b;
      int index = 0;
      int skip = 0;

      //Console.WriteLine("Before: " + String.Join(", ", hash));
      for (int r = 0; r < rounds; r++)
      {
        //Console.WriteLine("Round: " + r);
        for (int d = 0; d < data.Length; d++)
        {
          //Console.WriteLine("Length: " + data[l]);
          for (int i = 0; i < data[d]/2; i++)
          {
            a = index + i;
            b = index + data[d] - i - 1;

            while (a >= hash.Length) a -= hash.Length;
            while (b >= hash.Length) b -= hash.Length;

            if (a == b) break;

            //Console.WriteLine($"Change a[{a}] <-> b[{b}]: " + String.Join(", ", hash));
            tmp = hash[b];
            hash[b] = hash[a];
            hash[a] = tmp;
          }

          index += data[d] + skip;
          skip += 1;
        }
      }

      //Console.WriteLine("After: " + String.Join(", ", hash));
      return hash;
    }

    static int[] DenseHash(int[] hash, int blockSize = 16)
    {
      int[] denseHash = new int[hash.Length/blockSize];

      for (int i = 0; i < denseHash.Length; i++)
      {
        denseHash[i] = 0;
        for (int j = 0; j < blockSize; j++)
          denseHash[i] ^= hash[(i*blockSize)+j];
      }

      return denseHash;
    }

    static double Solve1(string input, int dataSize = 256)
    {
      Console.WriteLine($"Solve1 Raw Input: '{input}'");
      int[] lengths = input
        .Split(",")
        .Select(i => i.Trim())
        .Where(i => i != "")
        .Select(i => int.Parse(i))
        .ToArray();

      int[] result = Hash(lengths, dataSize);
      return result[0]*result[1];
    }

    static string Solve2(string input, int dataSize = 256)
    {
      int[] tail = new [] { 17, 31, 73, 47, 23 };

      Console.WriteLine($"Solve2 Raw Input: '{input}'");
      int[] bytes = input
        .ToCharArray()
        .Select(i => (int)i)
        .Concat(tail)
        .ToArray();

      int[] sparseHash = Hash(bytes, dataSize, 64);
      int[] denseHash = DenseHash(sparseHash);
      byte[] denseHashBytes = denseHash.Select(b => (byte)b).ToArray();
      return BitConverter.ToString(denseHashBytes).Replace("-", "").ToLower();
    }

  }
}
