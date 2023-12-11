import std.stdio;
import std.ascii;
import std.algorithm;
import std.conv;
import std.array;
import std.range;
import std.typecons;
import std.math;

alias Position = Tuple!(long, "x", long, "y");

char[][] parseInput(File input) => input.byLine!string(KeepTerminator.no, std.ascii.newline).map!dup.array;

Position[] getGalaxies(char[][] universe, int multiplier)
{
    int[][] tmp = universe.map!(row => row.map!(cell => cell == '.' ? 0 : 1).array).array;
    foreach (row; tmp[1..$])
    {
        tmp[0] = tmp[0].enumerate.map!(a => a.value + row[a.index]).array;
    };
    int[] emptyColumns = tmp[0].enumerate.filter!(a => a.value == 0).map!(a => a.index.to!int).array;

    Position[] galaxies;
    long y = 0;
    foreach (i, row; universe)
    {
        long x = 0;
        foreach (j, cell; row)
        {
            if (cell == '#')
            {
                galaxies ~= Position(x + j, y + i);
            }

            if (emptyColumns.countUntil(j) != -1)
            {
                x += multiplier - 1;
            }
        }

        if (row.countUntil('#') == -1)
        {
            y += multiplier - 1;
        }
    }
    return galaxies;
}

long solution(char[][] universe, int multiplier)
{
    Position[] galaxies = universe.getGalaxies(multiplier);
    return cartesianProduct(galaxies, galaxies).map!(pair => abs(pair[0].x - pair[1].x) + abs(pair[0].y - pair[1].y)).sum / 2;
}

long solutionEasy(char[][] universe) =>
    solution(universe, 2);

long solutionHard(char[][] universe) =>
    solution(universe, 1000000);

void main()
{
    File input = File("input.txt");
    char[][] galaxy = input.parseInput;
    galaxy.solutionEasy.writeln;
    galaxy.solutionHard.writeln;

}
