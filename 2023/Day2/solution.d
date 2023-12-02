import std.stdio;
import std.container : SList;
import std.array: split;
import std.conv: to;
import std.string : toStringz;
import std.ascii;
import std.algorithm : max;

struct Game
{
    int id;
    Draw[][] cubeBags;
}

struct Draw
{
    string color;
    int number;
}

bool isLimitExceeded(const ref Game game)
{
    foreach (const ref Draw[] draws; game.cubeBags)
    {
        foreach(const ref Draw draw; draws)
        {
            if ((draw.color == "blue" && draw.number > 14) ||
                (draw.color == "green" && draw.number > 13) ||
                (draw.color == "red" && draw.number > 12))
            {
                return true;
            }
        }
    }

    return false;
}

int getColorPower(const ref Game game)
{
    int blue, green, red;
    foreach (const ref Draw[] draws; game.cubeBags)
    {
        foreach(const ref Draw draw; draws)
        {
            if (draw.color == "blue") 
            {
                blue = max(blue, draw.number);
            }
            if (draw.color == "green") 
            {
                green = max(green, draw.number);
            }
            if (draw.color == "red") 
            {
                red = max(red, draw.number);
            }
        }
    }

    return blue * red * green;
}

int solutionEasy(const ref Game[] games)
{
    int sum;

    foreach (const ref Game game; games)
    {
        if (!isLimitExceeded(game))
        {
            sum += game.id;
        }
    }
    return sum;
}

int solutionHard(const ref Game[] games)
{
    int sum;

    foreach (const ref Game game; games)
    {
        sum += getColorPower(game);
    }
    return sum;
}

Game[] parseInput(File input)
{
    Game[] games;

    foreach (char[] line; input.byLine!(string)(KeepTerminator.no, std.ascii.newline))
    {   
        Draw[][] cubeBags;

        auto splitId = line.split(": ");
        auto splitBags = splitId[1].split("; ");
        foreach (char[] bag; splitBags)
        {
            Draw[] singleBag; 
            auto splitDraws = bag.split(", ");
            foreach (char[] draw; splitDraws)
            {
                char[][] splitDraw = draw.split(" ");
                singleBag ~= Draw(to!string(splitDraw[1]), number:to!int(splitDraw[0]));
            }
            cubeBags ~= singleBag;
        }
        
        games ~= Game(to!int(splitId[0][5..$]), cubeBags);
    }

    return games;
}

void main()
{
    auto input = File("input.txt");
    Game[] games = parseInput(input);
    writeln(solutionEasy(games));
    writeln(solutionHard(games));
}
