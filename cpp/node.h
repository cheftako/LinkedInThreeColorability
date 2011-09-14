#ifndef NODE_H
#define NODE_H

#include <string>
#include <iostream>
#include <set>
#include <ext/hash_map>
#include <fstream>

using namespace std;
namespace std { using namespace __gnu_cxx; }

// int getIndexForStringName(string name);
// string *getStringNameForIndex(int index);

struct eqstr{
  bool operator()(const char* s1, const char* s2) const {
    return strcmp(s1,s2)==0;
  }
};

class Node
{
private:
    string m_description;
    string m_name;
    int    m_index;
    int    m_vertexCount;
    // string *m_vertices;
    set<int> m_orderedVertices;

    void insertVertexIndex(int index);

public:
    Node();
    Node(string description, int index);
    ~Node();

    void determineVertices(hash_map<const char*, int, hash<const char*>, eqstr> nameIndexMap);
    string getDescription();
    string getName() { return m_name; }
    int getIndex() { return m_index; }
    int getVertexCount() { return m_vertexCount; }
    set<int>::iterator getVerticesEnd();
    set<int>::iterator getVerticesAtOrAfter(int index);
    set<int>::iterator getVertices();
    bool hasVertex(int index);
    string print();
};

#endif
