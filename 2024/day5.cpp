#include <regex>
#include <iostream>
#include <vector>
#include <fstream>
#include <map>

using namespace std;

const regex pageOrderRegex("([0-9]+)\\|([0-9]+)");
const regex numRegex("([0-9]+)");

struct PageOrder {
    int first;
    int last;
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

vector<PageOrder> CreatePageOrderTable(vector<string> lines) {
    vector<PageOrder> orders;

    for (size_t i = 0; i < lines.size(); i++)
    {
        string line = lines[i];
        vector<string> pageMatches = RegexMatches(line, pageOrderRegex);
        if(pageMatches.size() > 0) {
            vector<int> numbers = NumbersFromString(pageMatches[0]);
            orders.push_back({
                numbers[0],
                numbers[1]
            });
            continue;
        }
        break;
    }
    return orders;
}

vector<PageOrder> GetPageOrders(vector<PageOrder> allOrders, int page) {
    vector<PageOrder> orders;

    for (size_t i = 0; i < allOrders.size(); i++)
    {
        if(allOrders[i].first == page) orders.push_back(allOrders[i]);
    }
    
    return orders;
}

vector<vector<int>> CreateUpdates(vector<string> lines) {
    vector<vector<int>> pages;

    for (size_t i = 0; i < lines.size(); i++)
    {
        string line = lines[i];
        if(RegexMatches(line, pageOrderRegex).size() > 0) continue;
        
        vector<int> numbers = NumbersFromString(line);
        if(numbers.size() == 0) continue;

        pages.push_back(numbers);
    }
    return pages;
}

vector<vector<int>> FilterUpdates(vector<vector<int>> updates, vector<PageOrder> pageOrders, bool correctPages) {
    vector<vector<int>> correctUpdates;

    for (int u = 0; u < updates.size(); u++)
    {
        vector<int> update = updates[u];
        bool valid = true;
        for (int i = update.size() - 1; i >= 0; i--)
        {
            vector<PageOrder> relevantPages = GetPageOrders(pageOrders, update[i]);
            for (int j = 0; j < i; j++)
            {
                for (int p = 0; p < relevantPages.size(); p++)
                {
                    if(relevantPages[p].last == update[j]) {
                        valid = false;
                        break;
                    }
                }
                if(valid == false) {
                    break;
                }
            }
            if(valid == false) {
                break;
            }
        }
        if(valid == correctPages) {
            correctUpdates.push_back(update);
        }
    }
    return correctUpdates;
}

int CalculateUpdateMiddlePageSum(vector<vector<int>> updates) {
    int sum = 0;
    for (int i = 0; i < updates.size(); i++)
    {
        sum += updates[i][updates[i].size() / 2];
    }
    return sum;
}

vector<vector<int>> SortUpdatePages(vector<vector<int>> updates, vector<PageOrder> pageOrders) {
    vector<vector<int>> sorted;

    for (int u = 0; u < updates.size(); u++)
    {
        vector<int> update = updates[u];
        vector<int> s;
        s.push_back(update[0]);

        for (int i = 0; i < update.size(); i--)
        {
            vector<PageOrder> relevantPages = GetPageOrders(pageOrders, update[i]);
            for (int p = 0; p < relevantPages.size(); p++)
            {
                if(relevantPages[p].last == update[i]) {
                    int p = update[i];
                    update[i] = update[i-1];
                    update[i-1] = p;
                    break;
                }
            }
            // for (int j = 0; j < i; j++)
            // {
            //     for (int p = 0; p < relevantPages.size(); p++)
            //     {
            //         if(relevantPages[p].last == update[j]) {
            //             int p = update[i];
            //             update[i] = update[i-1];
            //             update[i-1] = p;
            //             break;
            //         }
            //     }
            // }
        }
        sorted.push_back(update);
    }
    
    return sorted;
}

int main() {
    vector<string> lines = ReadFileLines("./Input/day5.txt");
    vector<PageOrder> pageOrders = CreatePageOrderTable(lines);
    vector<vector<int>> updates = CreateUpdates(lines);
    vector<vector<int>> correctUpdates = FilterUpdates(updates, pageOrders, true);
    std::cout << "Part 1: " << CalculateUpdateMiddlePageSum(correctUpdates) << endl;

    vector<vector<int>> wrongUpdates = FilterUpdates(updates, pageOrders, false);
    vector<vector<int>> sortedUpdates = SortUpdatePages(wrongUpdates, pageOrders);
    std::cout << "Part 2: " << CalculateUpdateMiddlePageSum(sortedUpdates) << endl;
}
// 6066 - low
// 6547 - high