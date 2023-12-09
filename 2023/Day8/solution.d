import std.stdio;
import std.ascii;
import std.algorithm;
import std.conv;
import std.array;
import std.typecons;
import std.numeric;

alias Pair = Tuple!(string, "left", string, "right");

alias Network = Tuple!(string, "pattern", Pair[string], "mapping");

Network parseInput(File input)
{
    auto lines = input.byLine!string(KeepTerminator.no, std.ascii.newline).map!(to!string).array;
    Pair[string] mapping;
    foreach (line; lines[2..$])
    {
        string[] tokens = line.filter!(a => a != '(' && a != ')' && a != '=' && a != ',').array.to!string.split;
        mapping.require(tokens[0], Pair(tokens[1], tokens[2]));
    }
    return Network(lines[0], mapping);
}

string getNext(const ref Network network, string state) =>
    network.pattern.fold!((st, direction) => (direction == 'L' ? network.mapping[st].left : network.mapping[st].right))(state);


int solutionEasy(Network network, string start, string end) =>
    (start == end) ? 0 : solutionEasy(network, getNext(network, start), end) + network.pattern.length.to!int;

int solutionHardSingle(Network network, string start, string end) =>
    (start.back == end.back) ? 0 : solutionHardSingle(network, getNext(network, start), end) + network.pattern.length.to!int;

long solutionHard(Network network, string possibleStart, string end) 
{
    long[] singleResults = network.mapping.keys.filter!(key => key.back == possibleStart.back).map!(start => solutionHardSingle(network, start, end).to!long).array;
    return singleResults.fold!((acc, x) => lcm(acc, x))(1.to!long);
}

void main()
{
    File input = File("input.txt");
    Network network = input.parseInput;
    solutionEasy(network, "AAA", "ZZZ").writeln;
    solutionHard(network, "AAA", "ZZZ").writeln;
}
