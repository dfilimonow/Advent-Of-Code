import std.stdio;
import std.array;
import std.ascii;
import std.conv: to;
import std.algorithm: map, sum, filter, count, find;
import std.range : enumerate, retro;
import std.typecons;
import std.uni : isWhite;
import std.math : pow;

alias Card = Tuple!(int, "id", int[], "winningNumbers", int[], "numbers", ulong, "scratcheads");

Card transform(const ref char[] s)
{
    auto idSplit = s.split(": ");
    auto numSplit = idSplit[1].split(" | ");
    return Card (idSplit[0].split(" ")[$ - 1].to!int, 
                 numSplit[0].split!isWhite.filter!(s => s.length != 0).map!(to!int).array, 
                 numSplit[1].split!isWhite.filter!(s => s.length != 0).map!(to!int).array, 0);
}

Card[] parseInput(File input) => input.byLine!(string)(KeepTerminator.no, std.ascii.newline).map!(l => l.transform).array;

ulong solutionEasy(const Card[] cards) =>
    cards.map!(card => pow(2, card.numbers.filter!(num => !card.winningNumbers.find(num).empty).count - 1)).sum;

ulong solutionHard(Card[] cards)
{
    ulong assignAndGetCount(Card card)
    {
        cards[card.id - 1].scratcheads = 1;
        foreach (i; 1..(card.numbers.filter!(num => !card.winningNumbers.find(num).empty).count + 1))
        {
            if (card.id - 1 + i < cards.length)
            {
                cards[card.id - 1].scratcheads += cards[card.id - 1 + i].scratcheads;
            }
        }
        return cards[card.id - 1].scratcheads;
    }
    
    return cards.retro.map!(card => assignAndGetCount(card)).sum;
}

void main()
{
    auto input = File("input.txt");
    Card[] cards = input.parseInput;
    cards.solutionEasy.writeln;
    cards.solutionHard.writeln;
}