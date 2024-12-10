#include <regex>
#include <iostream>
#include <vector>
#include <fstream>
#include <map>
#include <unordered_set>

using namespace std;

const regex numRegex("([0-9]+)");
const regex guardRegex("(v)|(\\^)|(<)|(>)");
const regex stepRegex("(X)");


struct Position {
    int x;
    int y;

    Position(int x, int y) : x(x), y(y){

    }

    const Position operator+(const Position& p) {
        return Position(x + p.x, y + p.y);
    }
};
struct Transform {
    Position pos;
    Position dir;

    Transform(Position pos, Position dir) : pos(pos), dir(dir){

    }
};

vector<string> ReadFileLines(string path) {
    fstream file(path, ios::in);
    vector<string> lines;
    string str;

    while(file >> str) {
        lines.push_back(str);
    }
    
    file.close();
    return lines;
}

vector<string> RegexMatches(string str, regex regx) {
    vector<std::string> matches;

    sregex_iterator mulBegin = sregex_iterator(str.begin(), str.end(), regx);
    sregex_iterator mulEnd = sregex_iterator();

    for (regex_iterator i = mulBegin; i != mulEnd; i++)
    {
        smatch match = *i;
        matches.push_back(match.str());
    }

    return matches;
}

vector<int> NumbersFromString(string str) {
    vector<string> strNumbers = RegexMatches(str, numRegex);
    vector<int> numbers;
    for (size_t i = 0; i < strNumbers.size(); i++)
    {
        numbers.push_back(stoi(strNumbers[i]));
    }
    return numbers;
}

Position FindGuard(vector<string> lab) {
    for (size_t y = 0; y < lab.size(); y++)
    {
        if(RegexMatches(lab[y], guardRegex).size() <= 0) {
            continue;
        }

        for (size_t x = 0; x < lab[y].size(); x++)
        {
            if(lab[y][x] == '^') {
                return Position(x, y);
            }
        }
        
    }
    
    cout << "No guard found!" << endl;
    return Position(0, 0);
}

void WalkAndMarkMap(vector<string>& lab, Position p, bool& looping) {
    bool walking = true;
    Position dir(0, -1);
    unordered_set obstacleCollisions;

    while (walking)
    {
        lab[p.y][p.x] = 'X';
        Position nextP = p + dir;
        if(nextP.y >= lab.size() || nextP.y < 0 || nextP.x >= lab[nextP.y].size() ||nextP.x < 0) {
            walking = false;
            break;
        }

        if(lab[nextP.y][nextP.x] == '#') {
            Transform collision = Transform(p, dir);
            if(obstacleCollisions.count(collision)) {
                looping = true;
                break;
            } else {
                obstacleCollisions.insert(Transform(p, dir));
            }
            dir = Position(-dir.y, dir.x);
            nextP = p + dir;
        }

        p = nextP;
    }
}

int CountFootsteps(vector<string>& lab) {
    int steps = 0;

    for (size_t i = 0; i < lab.size(); i++)
    {
        steps += RegexMatches(lab[i], stepRegex).size();
    }
    
    return steps;
}

int main() {
    vector<string> lab = ReadFileLines("./Input/day6.txt");
    Position startPos = FindGuard(lab);
    bool looping = false;
    WalkAndMarkMap(lab, startPos, looping); // marking lab destructivly & can't be bothered to fix
    int steps = CountFootsteps(lab);
    cout << "Looping: " << looping << endl;
    cout << "Part 1: " << steps << endl;

    //lab = ReadFileLines("./Input/day6.txt"); // so reading file again instead

}