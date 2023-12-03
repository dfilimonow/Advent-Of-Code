import std.stdio;
import std.array;
import std.ascii;
import std.conv: to;
import std.algorithm: map, sum, filter;
import std.range : enumerate;
import std.typecons;

alias Gear = Tuple!(int, "count", int, "value");

string[] parseInput(File input) => input.byLine!(string)(KeepTerminator.no, std.ascii.newline).map!(to!string).array;

void solutions(const ref string[] schematic)
{
    auto width = schematic[0].length;
    auto height = schematic.length;

    Gear[ulong] gears;

    bool isSymbol(ulong x, ulong y) => (0 <= x && x < width) && (0 <= y && y < height) && !isDigit(schematic[y][x]) && schematic[y][x] != '.';

    bool isPartial(ulong x, ulong y, ulong length, int value) {
        for (long i = to!long(x) - length; i <= to!long(x) + 1; i++)
        {
            for (long j = to!long(y) - 1; j < to!long(y) + 2; j++) 
            {
                if (isSymbol(i, j)) {
                    if (schematic[j][i] == '*') {
                        gears.require(j * 1000 + i, Gear(0, 1));
                        gears[j * 1000 + i].count++;
                        gears[j * 1000 + i].value *= value;
                    }
                    return true;
                }
            }
        }

        return false;
    }

    int getNumbersInLine(string line, ulong lineNum) {
        char[] current;
        int sum;
        foreach(letterNum, letter; line.enumerate(0)) {
            if (isDigit(letter)) {
                current ~= letter;
                if (letterNum + 1 == width && isPartial(letterNum, lineNum, current.length, to!int(current))) 
                {
                    sum += to!int(current);
                }
            }
            else if (current.length > 0)
            {
                if (isPartial(letterNum - 1, lineNum, current.length, to!int(current))) {
                    sum += to!int(current);
                }
                current = [];
            }
            
        }

        return sum;
    }

    int sum;
    foreach (lineNum, line; schematic) {
        sum += getNumbersInLine(line, lineNum);
    }
    writeln(sum);
    writeln(gears.byValue().filter!(a => a.count == 2).map!(a => a.value).sum);

}

void main()
{
    auto input = File("input.txt");
    string[] schematic = parseInput(input);
    solutions(schematic);
}