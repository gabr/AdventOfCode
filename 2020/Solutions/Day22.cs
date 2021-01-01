using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Solutions
{
    public class Day22
    {

        public (Queue<int> Deck1, Queue<int> Deck2) GetPlayersDecks(string[] decks)
        {
            var deck1 = new Queue<int>();
            var deck2 = new Queue<int>();

            int index = 1;
            while (index < decks.Length && decks[index] != "")
                deck1.Enqueue(int.Parse(decks[index++]));

            index += 2;
            while (index < decks.Length && decks[index] != "")
                deck2.Enqueue(int.Parse(decks[index++]));

            return (deck1, deck2);
        }

        public int Solve1(string[] decks)
        {
            (var deck1, var deck2) = GetPlayersDecks(decks);

            //Console.WriteLine($"P1: {string.Join(", ", deck1)}");
            //Console.WriteLine($"P2: {string.Join(", ", deck2)}");

            while (deck1.Count > 0 && deck2.Count > 0)
            {
                var p1 = deck1.Dequeue();
                var p2 = deck2.Dequeue();

                if (p1 > p2)
                {
                    deck1.Enqueue(p1);
                    deck1.Enqueue(p2);
                }
                else
                {
                    deck2.Enqueue(p2);
                    deck2.Enqueue(p1);
                }
            }

            var winDeck = deck1.Count == 0 ? deck2 : deck1;

            int score = 0;
            while (winDeck.Count > 0)
            {
                int multiplier = winDeck.Count;
                score += (winDeck.Dequeue() * multiplier);
            }

            return score;
        }


        public static readonly string[] PUZZLE_INPUT =
        {
            "Player 1:",
            "6",
            "25",
            "8",
            "24",
            "30",
            "46",
            "42",
            "32",
            "27",
            "48",
            "5",
            "2",
            "14",
            "28",
            "37",
            "17",
            "9",
            "22",
            "40",
            "33",
            "3",
            "50",
            "47",
            "19",
            "41",
            "",
            "Player 2:",
            "1",
            "18",
            "31",
            "39",
            "16",
            "10",
            "35",
            "29",
            "26",
            "44",
            "21",
            "7",
            "45",
            "4",
            "20",
            "38",
            "15",
            "11",
            "34",
            "36",
            "49",
            "13",
            "23",
            "43",
            "12",
        };


    }
}

