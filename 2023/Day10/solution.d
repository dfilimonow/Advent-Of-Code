import std.stdio;
import std.ascii;
import std.algorithm;
import std.conv;
import std.array;
import std.range;
import std.typecons;
import std.string;

alias Position = Tuple!(long, "x", long, "y");

Position getNextPosition(ref char[][] maze, Position current, Position previous)
{
    switch (maze[current.y][current.x])
    {
        case '|':
            return current.y - 1 == previous.y ? Position(current.x, current.y + 1) : Position(current.x, current.y - 1);
        case '-':
            return current.x - 1  == previous.x ? Position(current.x + 1, current.y) : Position(current.x - 1, current.y);
        case 'L':
            return current.x + 1 == previous.x ? Position(current.x, current.y - 1) : Position(current.x + 1, current.y);
        case 'J':
            return current.x - 1 == previous.x ? Position(current.x, current.y - 1) : Position(current.x - 1, current.y);
        case '7':
            return current.x - 1 == previous.x ? Position(current.x, current.y + 1) : Position(current.x - 1, current.y);
        case 'F':
            return current.x + 1 == previous.x ? Position(current.x, current.y + 1) : Position(current.x + 1, current.y);
        default: 
            return Position(-1, -1);
    }
}

char[][] parseInput(File input) => input.byLine!string(KeepTerminator.no, std.ascii.newline).map!dup.array;

int traverse(ref char[][] maze, Position current, Position previous)
{
    int counter = 1; 
    while (maze[current.y][current.x] != 'S') 
    {   
        auto temp = current;
        current = getNextPosition(maze, current, previous);
        maze[temp.y][temp.x] = '#';
        previous = temp;
        counter++;
    }
    maze[current.y][current.x] = '#';
    return counter;
}

Position findAdjacent(ref char[][] maze, Position current)
{
    if (current.x - 1 >= 0 && (maze[current.y][current.x - 1] == '-' || maze[current.y][current.x - 1] == 'L' || maze[current.y][current.x - 1] == 'F'))
    {
        return Position(current.x - 1, current.y);
    } else if (current.y - 1 >= 0 && (maze[current.y - 1][current.x] == '|' || maze[current.y - 1] [current.x]== 'J' || maze[current.y - 1][current.x] == 'F'))
    {
        return Position(current.x, current.y - 1);
    } else {
        return Position(current.x + 1, current.y);
    }
}

int solutionEasy(ref char[][] maze)
{
    long[] location = maze.map!(a => a.countUntil('S')).array;
    Position startPosition = Position(location.maxElement, location.maxIndex);
    return traverse(maze, findAdjacent(maze, startPosition), startPosition) / 2;
}

char[] scaleUpLine(char[] line)
{
    char[] newLine;

    newLine ~= '.';
    newLine ~= line[0];
    for (int i = 0; i < line.length - 1; i++)
    {
        if (line[i] == '-' || line[i + 1] == '-' || 
           (line[i] == 'S' && line[i + 1] == '7') || 
           (line[i] == 'S' && line[i + 1] == 'J') || 
           (line[i] == 'F' && line[i + 1] == 'S') || 
           (line[i] == 'L' && line[i + 1] == 'S') || 
           (line[i] == 'F' && line[i + 1] == '7') || (line[i] == 'L' && line[i + 1] == 'J') ||  
           (line[i] == 'L' && line[i + 1] == '7') || (line[i] == 'F' && line[i + 1] == 'J'))
        {
            newLine ~= '-';
        } else {
            newLine ~= '.';
        }
        newLine ~= line[i + 1];
    }
    newLine ~= '.';
    return newLine;
}

char[][] scaleUp(char[][] maze)
{
    char[][] newMaze;

    maze[0] = maze[0].scaleUpLine;
    auto dots = maze[0].map!(a => '.').array;
    newMaze ~= dots.dup;
    newMaze ~= maze[0];
    for (int i = 0; i < maze.length - 1; i++)
    {
        char[] newLine;
        maze[i + 1] = maze[i + 1].scaleUpLine;
        for (int j = 0; j < maze[i].length; j++)
        {
            if (maze[i][j] == '|' || maze[i + 1][j] == '|' || 
               (maze[i][j] == 'S' && maze[i + 1][j] == 'L') || 
               (maze[i][j] == 'S' && maze[i + 1][j] == 'J') || 
               (maze[i][j] == 'F' && maze[i + 1][j] == 'S') || 
               (maze[i][j] == '7' && maze[i + 1][j] == 'S') || 
               (maze[i][j] == 'F' && maze[i + 1][j] == 'L') || (maze[i][j] == '7' && maze[i + 1][j] == 'J') ||
               (maze[i][j] == '7' && maze[i + 1][j] == 'L') || (maze[i][j] == 'F' && maze[i + 1][j] == 'J'))
            {
                newLine ~= '|';
            } else {
                newLine ~= '.';
            }
        }
        newMaze ~= newLine;
        newMaze ~= maze[i + 1];
    }
    newMaze ~= dots.dup;
    return newMaze;
}

char[][] floodFill(char[][] maze)
{
    Position[] stack;
    stack ~= Position(0, 0);

    while (!stack.empty)
    {
        Position top = stack.back;
        stack.popBack;
        maze[top.y][top.x] = '@';

        if (top.y - 1 >= 0 && maze[top.y - 1][top.x] == '.')
        {
            stack ~= Position(top.x, top.y - 1);
        }
        if (top.x - 1 >= 0 && maze[top.y][top.x - 1] == '.')
        {
            stack ~= Position(top.x - 1, top.y);
        }
        if (top.y + 1 < maze.length && maze[top.y + 1][top.x] == '.')
        {
            stack ~= Position(top.x, top.y + 1);
        }
        if (top.x + 1 < maze[0].length && maze[top.y][top.x + 1] == '.')
        {
            stack ~= Position(top.x + 1, top.y);
        }
    }

    return maze;
}

char[] scaleDownLine(char[] line)
{
    char[] newLine;
    for (int i = 1; i < line.length - 1; i += 2)
    {
        newLine ~= line[i];
    }
    return newLine;
}

char[][] scaleDown(char[][] maze)
{
    char[][] newMaze;

    for (int i = 1; i < maze.length - 1; i += 2)
    {
        newMaze ~= maze[i].scaleDownLine;
    }
    return newMaze;
}

ulong solutionHard(ref char[][] maze, char[][] originalMaze) =>
    maze.zip(originalMaze)
        .map!(lines => lines[0].zip(lines[1]).map!(c => c[0] == '#' ? c[1] : '.').map!(to!char).array).array
        .scaleUp
        .floodFill
        .scaleDown
        .map!(line => line.count('.'))
        .sum;

void main()
{
    File input = File("input.txt");
    char[][] maze = input.parseInput;
    char[][] originalMaze = maze.map!dup.array;
    maze.solutionEasy.writeln;
    solutionHard(maze, originalMaze).writeln;
}
