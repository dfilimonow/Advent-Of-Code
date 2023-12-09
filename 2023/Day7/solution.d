import std.stdio;
import std.ascii;
import std.algorithm;
import std.conv;
import std.array;
import std.typecons;
import std.numeric;
import std.range;

alias Hand = Tuple!(string, "cards", long, "bid");
alias EvaluatedHand = Tuple!(long, "value", long, "bid");

char[] ordering = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'];
char[] wildcardOrdering = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'];

Hand[] parseInput(File input) => 
    input.byLine!string(KeepTerminator.no, std.ascii.newline).map!(l => Hand(l.split[0].to!string, l.split[1].to!long)).array;

EvaluatedHand evaluate(Hand hand, bool isWildcardUsed = false)
{
    auto grouped = hand.cards.array.sort.group.array.sort!"a[1] > b[1]";
    
    if (isWildcardUsed)
    {
        auto jokerLocation = grouped.countUntil!"a[0] == 'J'";
        if (jokerLocation == 0 && grouped.length > 1)
        {
            grouped[1][1] += grouped[jokerLocation][1];
            grouped[jokerLocation][1] = 0;
        } else if (jokerLocation > 0)
        {
            grouped[0][1] += grouped[jokerLocation][1];
            grouped[jokerLocation][1] = 0;
        }
        grouped = grouped.sort!"a[1] > b[1]";
    }

    int highestCount = grouped[0][1];

    if (highestCount > 3)
    {
        highestCount += 2;
    } else if (highestCount > 2)
    {
        highestCount++;
    }
    if (grouped[0][1] == 3 && grouped[1][1] == 2) 
    {
        highestCount = 5;
    } else if (grouped[0][1] == 2 && grouped[1][1] == 2) 
    {
        highestCount = 3;
    }
    auto currentOrdering = isWildcardUsed ? wildcardOrdering : ordering;
    return EvaluatedHand(hand.cards.retro.enumerate.fold!((value, card) => value + (countUntil(currentOrdering, card[1]) + 1L) * ordering.length ^^ (highestCount - 1 + card[0]))(0L), hand.bid);
}

long solutionEasy(Hand[] hands) =>
    hands.map!evaluate.array.sort!("a.value < b.value").enumerate(1).fold!((sum, hand) => sum + hand[0] * hand[1].bid)(0L);

long solutionHard(Hand[] hands) =>
    hands.map!(hand => evaluate(hand, true)).array.sort!("a.value < b.value").enumerate(1).fold!((sum, hand) => sum + hand[0] * hand[1].bid)(0L);    

void main()
{
    File input = File("input.txt");
    Hand[] hands = input.parseInput;
    hands.solutionEasy.writeln;
    hands.solutionHard.writeln;
}
