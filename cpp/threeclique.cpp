#include <string>
#include <sstream>
#include <string.h>
#include "node.h"
#include "threeclique.h"

ThreeClique::ThreeClique(Node *nodeA, Node *nodeB, Node *nodeC)
{
    m_nodeA = nodeA;
    m_nodeB = nodeB;
    m_nodeC = nodeC;
    int vertexCount = 0;
    vertexCount += m_nodeA->getVertexCount();
    vertexCount += m_nodeB->getVertexCount();
    vertexCount += m_nodeC->getVertexCount();
    m_vertexCount = vertexCount;
}

ThreeClique::ThreeClique()
{
}

ThreeClique::~ThreeClique()
{
}

string ThreeClique::print()
{
    stringstream result;
    result << "ThreeClique (" << m_nodeA->getName() << ", " << m_nodeB->getName() << ", " << m_nodeC->getName() << ") has a vertex count of " << m_vertexCount << endl;
    return result.str();

}

bool operator<(ThreeClique a, ThreeClique b)
{
    if (a.getNodeA()->getName() < b.getNodeA()->getName())
    {
        return true;
    }
    else if (a.getNodeA()->getName() > b.getNodeA()->getName())
    {
        return false;
    }
    if (a.getNodeB()->getName() < b.getNodeB()->getName())
    {
        return true;
    }
    else if (a.getNodeB()->getName() > b.getNodeB()->getName())
    {
        return false;
    }
    if (a.getNodeC()->getName() < b.getNodeC()->getName())
    {
        return true;
    }
    else if (a.getNodeC()->getName() > b.getNodeC()->getName())
    {
        return false;
    }
    return false;
}

bool operator==(ThreeClique a, ThreeClique b)
{
    if (a.getNodeA()->getName() != b.getNodeA()->getName())
    {
        return false;
    }
    if (a.getNodeB()->getName() != b.getNodeB()->getName())
    {
        return false;
    }
    if (a.getNodeC()->getName() != b.getNodeC()->getName())
    {
        return false;
    }
    return true;
}

