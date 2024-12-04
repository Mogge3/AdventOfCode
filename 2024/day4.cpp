#include <iostream>
#include <vector>
#include <fstream>
#include <regex>

const std::regex xmasRegex("(XMAS)+|(SAMX)+");

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

std::vector<std::string> RegexMatches(std::string str, std::regex regx) {
    std::vector<std::string> matches;

    std::sregex_iterator mulBegin = std::sregex_iterator(str.begin(), str.end(), regx);
    std::sregex_iterator mulEnd = std::sregex_iterator();

    for (std::regex_iterator i = mulBegin; i != mulEnd; i++)
    {
        std::smatch match = *i;
        matches.push_back(match.str());
    }

    return matches;
}

std::vector<std::string> RegexLineMatches(std::vector<std::string> lines, std::regex regx) {
    std::vector<std::string> foundMultipliers;

    for (size_t i = 0; i < lines.size(); i++)
    {
        std::string line = lines[i];

        std::vector<std::string> lineMatches = RegexMatches(line, regx);
        foundMultipliers.insert(foundMultipliers.end(), lineMatches.begin(), lineMatches.end());
    }

    return foundMultipliers;
}

int main()
{
    std::vector<std::string> lines = ReadFileLines("../Input/4.txt");

    // Part 1
    std::vector<std::string> lineMatches = RegexLineMatches(lines, xmasRegex);
    std::cout << "RESULT: " << lineMatches.size() << std::endl;

//     Part 2
//     std::vector<std::string> instructions = RegexLineMatches(lines, instructionsRegex);
//     multipliers = ValidMultipliers(instructions);
//     multiplierTotal = CalculateMultipliers(multipliers);
//     std::cout << "RESULT: " << multiplierTotal << std::endl;
}