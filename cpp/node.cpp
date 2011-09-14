#include <string>
#include <sstream>
#include <string.h>
#include "node.h"

/*
int getIndexForStringName(string name)
{
    int index = 0;
    for (unsigned int pos = 0; pos < name.length(); pos++)
    {
       index *= 26;
       char letter = name[pos];
       if ('A' <= letter && letter <= 'Z')
       {
           index += letter - 'A' + 1;
       }
       else if ('a' <= letter && letter <= 'z')
       {
           index += letter - 'a' + 1;
       }
       else
       {
           return -1;
       }
    }
    return index - 1;
}

string *getStringNameForIndex(int index)
{
    string *result = new string();
    while (index / 26 > 0)
    {
        char letter = (index % 26) + 'A';
        result->insert(0, 1, letter);
        index = index / 26;
    }
    char letter = (index % 26) + 'A';
    result->insert(0, 1, letter);
    return result;
}
*/

void Node::insertVertexIndex(int index)
{
   m_orderedVertices.insert(index);
}

Node::Node(string description, int array_index)
{
    int index = description.find(":");
    m_name = description.substr(0, index);
    // m_index = getIndexForStringName(m_name);
    m_index = array_index;
    m_description = description;
    m_vertexCount = 0;
}

Node::Node()
{
}

Node::~Node()
{
/*
    if (m_vertices != NULL)
    {
        delete [] m_vertices;
        m_vertices = NULL;
    }
*/
}

void Node::determineVertices(hash_map<const char*, int, hash<const char*>, eqstr> nameIndexMap)
{
    int index = m_description.find(":");
    int length = m_description.length();
    string verticies = m_description.substr(index + 1, length - index);
    size_t endIndex = index;
    while (endIndex != string::npos)
    {
        endIndex = m_description.find(",", index + 1);
        string vertex = m_description.substr(index + 1, endIndex - index - 1);
        // int vertexIndex = getIndexForStringName(vertex);
        int vertexIndex = nameIndexMap[vertex.c_str()];
        insertVertexIndex(vertexIndex);
        // m_vertices[cntr++] = vertex;
        m_vertexCount++;
        index = endIndex;
    }
    endIndex = index = m_description.find(":");
}

string Node::getDescription()
{
    return m_description;
}

/*
bool Node::hasVertex(string name)
{
    for (int cntr = 0; cntr < m_vertexCount; cntr++)
    {
        if (name.compare(m_vertices[cntr]) == 0)
        {
            return true;
        }
    }
    return false;
}
*/

bool Node::hasVertex(int index)
{
    set<int>::iterator i = m_orderedVertices.find(index);
    return i != m_orderedVertices.end();
}

set<int>::iterator Node::getVerticesAtOrAfter(int index)
{
    set<int>::iterator result = m_orderedVertices.lower_bound(index);
    return result;
}

set<int>::iterator Node::getVertices()
{
    set<int>::iterator result = m_orderedVertices.begin();
    return result;
}

set<int>::iterator Node::getVerticesEnd()
{
    set<int>::iterator result = m_orderedVertices.end();
    return result;
}

string Node::print()
{
    stringstream result;
    result << "Name is " << m_name << endl << "Index is " << m_index << endl;
    set<int>::iterator iter = m_orderedVertices.begin();
    int cntr = 0;
    while (iter != m_orderedVertices.end())
    {
        result << "Vertex #" << cntr++ << " connects to " << *iter << endl;
    }
    result << endl;
    return result.str();
}
