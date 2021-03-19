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

        public int CalculateDecsScore(Queue<int> deck)
        {
            int score = 0;
            int deckSize = deck.Count;

            foreach (var card in deck)
            {
                score += (card * deckSize);
                deckSize -= 1;
            }

            return score;
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
            return CalculateDecsScore(winDeck);
        }

        public int Solve2(string[] decks)
        {
            (var deck1, var deck2) = GetPlayersDecks(decks);

            string CreateDecksSnapshot(Queue<int> d1, Queue<int> d2) =>
                string.Join(' ', d1) + " | " + string.Join(' ', d2);

            // returns true if deck1 wins
            bool Play(Queue<int> d1, Queue<int> d2)
            {
                var decksHistory = new HashSet<string>();
                while (d1.Count > 0 && d2.Count > 0)
                {
                    var snapshot = CreateDecksSnapshot(d1, d2);
                    if (decksHistory.Contains(snapshot))
                        return true;

                    decksHistory.Add(snapshot);

                    var p1 = d1.Dequeue();
                    var p2 = d2.Dequeue();

                    bool p1wins = p1 > p2;
                    if (d1.Count >= p1 && d2.Count >= p2)
                        p1wins = Play(
                            new Queue<int>(d1.Take(p1)),
                            new Queue<int>(d2.Take(p2)));

                    if (p1wins)
                    {
                        d1.Enqueue(p1);
                        d1.Enqueue(p2);
                    }
                    else
                    {
                        d2.Enqueue(p2);
                        d2.Enqueue(p1);
                    }
                }

                return d1.Count > 0;
            }

            bool player1win = Play(deck1, deck2);
            return CalculateDecsScore(player1win ? deck1 : deck2);
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

