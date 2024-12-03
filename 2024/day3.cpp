#include <iostream>
#include <vector>
#include <fstream>
#include <iterator>
#include <sstream>

int main()
{
    

    std::cout << "Hello World" << std::endl;
}

std::vector<std::string> ReadFile(std::string path) {
    std::fstream file;
    file.open(path, std::ios::in);

    std::string line;
    if (file.is_open()) {
        while(std::getline(file, line)) {
            std::istringstream ss(line);
        }
    }
}