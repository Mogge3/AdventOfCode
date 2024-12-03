#include <iostream>
#include <vector>
#include <fstream>
#include <regex>


const std::regex enableRegex("(do\\(\\))");
const std::regex disableRegex("(don't\\(\\))");
const std::regex mulRegex("(mul\\([0-9]+,[0-9]+\\))");
const std::regex instructionsRegex("(mul\\([0-9]+,[0-9]+\\))|(do\\(\\))|(don't\\(\\))");

const std::regex numRegex("([0-9]+)");

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

int CalculateMultiplier(std::string multiplier) {
    std::vector<std::string> strNumbers = RegexMatches(multiplier, numRegex);
    int numA = stoi(strNumbers[0]);
    int numB = stoi(strNumbers[1]);
    int total = numA * numB;

    return total;
}

int CalculateMultipliers(std::vector<std::string> multipliers) {
    int result = 0;

    for (size_t i = 0; i < multipliers.size(); i++)
    {
        std::string multiplier = multipliers[i];
        result += CalculateMultiplier(multiplier);
    }

    return result;
}

std::vector<std::string> ValidMultipliers(std::vector<std::string> instructions) {
    std::vector<std::string> multipliers;

    bool disable = false;
    for (size_t i = 0; i < instructions.size(); i++)
    {
        std::string instruction = instructions[i];

        if(std::regex_search(instruction, disableRegex)) {
            disable = true;
            continue;
        }
        if(std::regex_search(instruction, enableRegex)) {
            disable = false;
            continue;
        }

        if(!disable) {
            multipliers.push_back(instruction);
        }
    }
    
    return multipliers;
}

int main()
{
    std::vector<std::string> lines = ReadFileLines("../Input/3.txt");

    // Part 1
    std::vector<std::string> multipliers = RegexLineMatches(lines, mulRegex);
    int multiplierTotal = CalculateMultipliers(multipliers);
    std::cout << "RESULT: " << multiplierTotal << std::endl;

    // Part 2
    std::vector<std::string> instructions = RegexLineMatches(lines, instructionsRegex);
    multipliers = ValidMultipliers(instructions);
    multiplierTotal = CalculateMultipliers(multipliers);
    std::cout << "RESULT: " << multiplierTotal << std::endl;
}