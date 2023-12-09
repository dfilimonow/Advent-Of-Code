import std.stdio;
import std.ascii;
import std.algorithm;
import std.conv;
import std.array;

int[][] parseInput(File input) => input.byLine!(string)(KeepTerminator.no, std.ascii.newline).map!(a => a.split.map!(to!int).array).array;

int[] reduce(int[] source)
{   
    int[] target;
    foreach(i; 1..source.length)
    {
        target ~= source[i] - source[i - 1];
    }
    return target;
}

int extrapolate(int[] sequence) => all!"a == 0"(sequence) ? 0 : sequence[$ - 1] + sequence.reduce.extrapolate;

int extrapolateBackwards(int[] sequence) => all!"a == 0"(sequence) ? 0 : sequence[0] + sequence.reverse.reduce.extrapolate;

int solutionEasy(int[][] histories) =>
    histories.map!(extrapolate).sum;

int solutionHard(int[][] histories) =>
    histories.map!(extrapolateBackwards).sum;

void main()
{
    File input = File("input.txt");
    int[][] histories = input.parseInput;
    histories.solutionEasy.writeln;
    histories.solutionHard.writeln;
}
