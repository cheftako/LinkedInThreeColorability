#ifndef THREE_CLIQUE_H
#define THREE_CLIQUE_H

#include <string>
#include <iostream>
#include <set>
#include <fstream>
#include "node.h"
using namespace std;

class ThreeClique
{
private:
    Node   *m_nodeA;
    Node   *m_nodeB;
    Node   *m_nodeC;
    int    m_vertexCount;

public:
    ThreeClique();
    ThreeClique(Node *nodeA, Node *nodeB, Node *nodeC);
    ~ThreeClique();

    ThreeClique &operator=(ThreeClique extra)
    {
        m_nodeA = extra.m_nodeA;
        m_nodeB = extra.m_nodeB;
        m_nodeC = extra.m_nodeC;
        m_vertexCount = extra.m_vertexCount;
        return *this;
    }

    Node *getNodeA() { return m_nodeA; }
    Node *getNodeB() { return m_nodeB; }
    Node *getNodeC() { return m_nodeC; }
    int getVertexCount() { return m_vertexCount; }
    string print();
};

#endif
