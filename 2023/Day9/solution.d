import std.stdio;
import std.ascii;
import std.algorithm;
import std.conv;
import std.array;
import std.range;

int[][] parseInput(File input) => input.byLine!string(KeepTerminator.no, std.ascii.newline).map!(a => a.split.map!(to!int).array).array;

int[] reduce(int[] source) => source.zip(source.dropOne).map!(t => t[1] - t[0]).array;

int extrapolate(int[] sequence) => all!"a == 0"(sequence) ? 0 : sequence.back + sequence.reduce.extrapolate;

int extrapolateBackwards(int[] sequence) => all!"a == 0"(sequence) ? 0 : sequence.front + sequence.reverse.reduce.extrapolate;

int solutionEasy(int[][] histories) =>
    histories.map!extrapolate.sum;

int solutionHard(int[][] histories) =>
    histories.map!extrapolateBackwards.sum;

void main()
{
    File input = File("input.txt");
    int[][] histories = input.parseInput;
    histories.solutionEasy.writeln;
    histories.solutionHard.writeln;
}
