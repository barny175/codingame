#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <tuple>

using namespace std;

void readLine(int size, vector<int>& result) {
    for (int i = 0; i < size; i++) {
        int val;
        cin >> val; cin.ignore();
        result.push_back(val);
    }
}

const char* creatures = ".GZV";

enum Direction {
    Up = 0,
    Left = 1,
    Down = 2,
    Right = 3
};

struct Position {
    int row;
    int col;
    Position(const Position& p) { 
        row = p.row;
        col = p.col;
    }
    Position(int r, int c) {
        row = r;
        col = c;
    }
};

enum Bool {
    True,
    False,
    NotDecidedYet
};

void printVec(const vector<int>& v, ostream& out) {
    copy (v.begin(), v.end(), ostream_iterator<int>(out, ", "));
    out << endl;
}

class Manor {
    int size;
    char** manor;
    vector<int> canSeeFromTop;
    vector<int> canSeeFromBottom;
    vector<int> canSeeFromLeft;
    vector<int> canSeeFromRight;
    int vampireCount;
    int zombieCount;
    int ghostCount;
    int *counts;
    vector<tuple<int, int> > emptyPlaces;
    
    Position initial_pos(int rowOrCol, Direction direction) {
        switch (direction) {
            case Up:
                return Position(size - 1, rowOrCol);
            case Down:
                return Position(0, rowOrCol);
            case Left:
                return Position(rowOrCol, size - 1);
            case Right:
                return Position(rowOrCol, 0);
        }
    }
    
    bool isValid(Position pos) {
        return pos.row >= 0 && pos.row < size && pos.col >= 0 && pos.col < size;
    }
    
    Direction mirror(Direction dir, char mirrorType) {
        if((mirrorType == '/' && (dir == Up || dir == Down))
            || (mirrorType == '\\' && (dir == Left || dir == Right))) {
            return static_cast<Direction>((dir - 1 + 4) % 4);
        } else {
            return static_cast<Direction>((dir + 1) % 4);
        }
    }
    
    tuple<Position, Direction> move(Position pos, Direction direction) {
        
        // cerr << "move from " << pos.row << " " << pos.col << ", dir " << direction << endl;
        if (isMirror(pos)) {
            direction = mirror(direction, manor[pos.row][pos.col]);
            // cout << "mirror: " << manor[pos.col][pos.row] << " dir: " << direction << " new dir: " << direction << endl;
        }
        
        Position newPos(pos);
        switch (direction) {
            case Up:
                newPos.row = pos.row - 1;
                break;
            case Down:
                newPos.row = pos.row + 1;
                break;
            case Left:
                newPos.col = pos.col - 1;
                break;
            case Right:
                newPos.col = pos.col + 1;
                break;
        } 
            
        return make_tuple(newPos, direction);
    }
    
    bool isMirror(Position pos) {
        char c = manor[pos.row][pos.col];   
        return (c == '/' || c == '\\');
    }
    
    bool isVisible(Position pos, bool mirror) {
        char creature = manor[pos.row][pos.col];
        //cerr << "row: " << pos.row << " col: " << pos.col << " creature: " << creature << " mirror: " << mirror << endl;
        switch (creature) {
            case 'V':
                return !mirror;
            case 'G':
                return mirror;
            case 'Z':
                return true;
            default:
                return false;
        }
    }
    
    int countCreatures(int rowOrCol, Direction direction) {
        bool end = false;
        bool mirror = false;
        int creatures = 0;
        Direction currDir = direction;
        Position pos = initial_pos(rowOrCol, direction);
        //cerr << "count creatures at " << rowOrCol << endl;
        while (isValid(pos)) {
            if(manor[pos.row][pos.col] == '.')
                return -1;
                
            if (isVisible(pos, mirror))
                creatures++;
                
            if (isMirror(pos))
                mirror = true;
                
            tie(pos, currDir) = move(pos, currDir);
            //cerr << "new pos: " << pos.row << " " << pos.col << " , dir: " << currDir << endl;
        }
        return creatures;
    }

    Bool vecEqual(vector<int> v1, vector<int> v2) {
        if (v1.size() != v2.size())
            return False;
            
        for (int i = 0; i < v1.size(); i++) {
            if (v1[i] == -1 || v2[i] == -1)
                return NotDecidedYet;
                
            if (v1[i] != v2[i])
                return False;
        }
        return True;
    }
    
    Bool check() {
        Bool res = vecEqual(canSeeFrom(Left), canSeeFromRight);
        if (res != True)
            return res;
        
        res = vecEqual(canSeeFrom(Up), canSeeFromBottom);
        if (res != True)
            return res;
        
        res = vecEqual(canSeeFrom(Down), canSeeFromTop);
        if (res != True)
            return res;
        return vecEqual(canSeeFrom(Right), canSeeFromLeft);
    }    
public:
    Manor() {
        cin >> vampireCount >> zombieCount >> ghostCount; cin.ignore();
        cin >> size; cin.ignore();
        readLine(size, canSeeFromTop);
        readLine(size, canSeeFromBottom);
        readLine(size, canSeeFromLeft);
        readLine(size, canSeeFromRight);
        
        int empty = 0;
        manor = new char*[size];
        for (int i = 0; i < size; i++) {
            manor[i] = new char[size];
            for (int j = 0; j < size; j++) {
                cin >> manor[i][j];
                if (manor[i][j] == '.') {
                    empty++;
                    emptyPlaces.push_back(make_tuple(i, j));
                }
            }
        }

        counts = new int[4] { empty - ghostCount - zombieCount - vampireCount, ghostCount, zombieCount, vampireCount };
    }
    
    vector<int> canSeeFrom(Direction dir) {
        vector<int> result;
        for (int i = 0; i < size; i++) {
            result.push_back(countCreatures(i, dir));
        }
        return result;
    }
    
    void print() {
        for (int i = 0; i < size; i++) {
            cout << manor[i];
            cout << endl;
        }
    }
    
    Bool find(int level = 0) {
        if (level == emptyPlaces.size() || level > 3) {
            Bool res = check();
            
            if (res == True) {
                print();
                return True;
            } else if (res == False) {
                cerr << res << " " << level << endl;
                return res;
            }
        }
        
        int r, c;
        tie(r, c) = emptyPlaces[level];
        // cerr << "level: " << level << " r: " << r << " c: " << c << endl;    
    
        for (int k = 0; k < 4; k++) {
            if (counts[k] > 0) {
                counts[k]--;
                manor[r][c] = creatures[k];
                //cerr << " k: " << k << "['" << creatures[k] << "']" << endl;
                Bool res = find(level + 1);
                if (res == True) {
                    return True;
                } 
                counts[k]++;
                manor[r][c] = '.';
            }
        }
        return False;
    }

};

int main()
{
    Manor manor;
    manor.find();
}