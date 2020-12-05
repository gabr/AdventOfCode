using System;
using System.Collections.Generic;

namespace Solutions
{
    public class Day09
    {
        public long Solve(int playersCount, int lastMarbleValue)
        {
            var marbles = new LinkedList<long>();
            var players = new long[playersCount];

            long currentMarbleValue = 0;
            int currentPlayerIndex  = 0;
            long maxPlayerScore     = 0;

            // setup
            var currentMarble = marbles.AddFirst(0);
            currentMarbleValue = 1;

            while (currentMarbleValue <= lastMarbleValue)
            {
                if (currentMarbleValue % 23 == 0)
                {
                    for (int i = 0; i < 7; i++)
                    {
                        if (currentMarble.Previous == null)
                            currentMarble = marbles.Last;
                        else
                            currentMarble = currentMarble.Previous;
                    }

                    players[currentPlayerIndex] += currentMarbleValue + currentMarble.Value;
                    var marbleToRemove = currentMarble;

                    if (currentMarble.Next == null)
                        currentMarble = marbles.First;
                    else
                        currentMarble = currentMarble.Next;

                    marbles.Remove(marbleToRemove);

                    if (players[currentPlayerIndex] > maxPlayerScore)
                        maxPlayerScore = players[currentPlayerIndex];
                }
                else
                {
                    if (currentMarble.Next == null)
                        currentMarble = marbles.First;
                    else
                        currentMarble = currentMarble.Next;

                    currentMarble = marbles.AddAfter(currentMarble, currentMarbleValue);
                }

                currentMarbleValue += 1;
                currentPlayerIndex = (currentPlayerIndex + 1) % playersCount;
            }

            return maxPlayerScore;
        }
    }
}

