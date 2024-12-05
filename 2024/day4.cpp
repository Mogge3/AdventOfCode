#include <iostream>
#include <vector>
#include <fstream>
#include <regex>
#include <array>

typedef long long LL;
const std::regex xmasRegex("(XMAS)+|(SAMX)+");

struct Point_2D
{
long long x_;
long long y_;
};

constexpr Point_2D DIRS_2D_8[8] = {
    { -1, -1 }, { 0, -1 }, { 1, -1 },
    { -1,  0 },            { 1,  0 },
    { -1,  1 }, { 0,  1 }, { 1,  1 }
};

std::vector<std::string> ReadFileLines(std::string path) {
    std::fstream file(path, std::ios::in);
    std::vector<std::string> lines;
    std::string str;

    while(file >> str) {
        lines.push_back(str);
    }
    
    file.close();
    return lines;
}

void PartOne(std::vector<std::string> map) {
    int p1_total = 0;
    constexpr std::array<char, 4> xmas = { 'X', 'M', 'A', 'S' };
    for (LL y = 0; y < map.size(); ++y) {
        for (LL x = 0; x < map[0].size(); ++x) {
            if (map[y][x] == 'X') {
                for (Point_2D offset : DIRS_2D_8) {
                    bool pass = true;
                    for (int letter = 1; pass && letter < xmas.size(); ++letter) {
                        LL new_x = x + (letter * offset.x_);
                        LL new_y = y + (letter * offset.y_);
                        if (new_x < 0 || new_x >= map[0].size() ||
                            new_y < 0 || new_y >= map.size()    ||
                            map[new_y][new_x] != xmas[letter]) {
                            pass = false;
                        }
                    }
                    if (pass) {
                        ++p1_total;
                    }
                }
            }
        }
    }
    std::cout << "PART ONE: " << p1_total << '\n';
}

void PartTwo(std::vector<std::string> map) {
    int p2_total = 0;
    for (int y = 1; y < map.size() - 1; ++y) {
        for (int x = 1; x < map[0].size() - 1; ++x) {
            if (map[y][x] == 'A') {
                int count = 0;
                constexpr std::array<int, 4> corners = { 0, 2, 5, 7 };
                for (int corner_i = 0, dir = 0;
                    count < 2 && corner_i < 4;
                    dir = corners[++corner_i]) {
                        auto [ x_offset, y_offset ] = DIRS_2D_8[dir];
                        if (map[y +  y_offset][x +  x_offset] == 'M' &&
                            map[y + -y_offset][x + -x_offset] == 'S') {
                            ++count;
                        }
                    }
                    if (count == 2) {
                    ++p2_total;
                }
            }
        }
    }
    std::cout << "PART TWO: " << p2_total << '\n';
}

int main()
{
    std::vector<std::string> lines = ReadFileLines("./Input/day4.txt");
    PartOne(lines);
    PartTwo(lines);
}