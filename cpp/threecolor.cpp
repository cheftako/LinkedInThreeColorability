#include <iostream>
#include <fstream>
#include <sstream>
#include <pthread.h>
#include <vector>
#include <map>
#include <ext/hash_map>
#include <set>
#include <string.h>
#include "node.h"
#include "threeclique.h"

#undef DEBUG_WANTED 
// #define DEBUG_WANTED 
#ifdef DEBUG_WANTED
#define DEBUG(msg...) { printf(msg); printf("\n"); }
#else
#define DEBUG(msg...) { }
#endif

#undef TIMER_WANTED 
// #define TIMER_WANTED 
#ifdef TIMER_WANTED
#define TIMER(id) { struct tm *current; time_t now; time(&now); current = localtime(&now); printf("Timer for %s is %i:%i:%i\n", id, current->tm_hour, current->tm_min, current->tm_sec); }
#else
#define TIMER(msg...) { }
#endif

namespace std { using namespace __gnu_cxx; }

unsigned int g_count = 0;

static const int SUCCESS = 1;
static const int FAILED_CLIQUE_CHECK = -1;

enum Color { UNKNOWN = 0, RED = 1, GREEN = 2, BLUE = 4, NOT_RED = 8, NOT_GREEN = 16, NOT_BLUE = 32, NOT_RED_OR_GREEN = 24, NOT_RED_OR_BLUE = 40, NOT_GREEN_OR_BLUE = 48, IMPOSSIBLE = 56 };

enum Solution { UNKNOWN_SOLUTION = 0, SOLVEABLE = 1, NOT_COLORABLE = 2 };

Solution solution = UNKNOWN_SOLUTION;
Color* colorSolution;
pthread_mutex_t done_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  done_cond  = PTHREAD_COND_INITIALIZER;

class colorexception: public exception
{
  virtual const char* what() const throw()
  {
    return "Detected a conflict during coloring";
  }
} colorex;

int fixColor(int color)
{
    switch (color)
    {
    case 1:
        return 1;
    case 2:
        return 2;
    case 4:
        return 3;
    default:
        return -1;
    }
}

void setSolution(Solution localSolution, Color colors[])
{
    pthread_mutex_lock(&done_mutex);
    if (solution != UNKNOWN_SOLUTION)
    {
        pthread_mutex_unlock(&done_mutex);
        return;
    }
    if (localSolution == SOLVEABLE)
    {
        solution = SOLVEABLE;
        for (unsigned int cntr = 0; cntr < g_count; cntr++)
        {
            colorSolution[cntr] = colors[cntr];
        }
    }
    else if (localSolution == NOT_COLORABLE)
    {
        solution = NOT_COLORABLE;
    }
    pthread_cond_signal(&done_cond);
    pthread_mutex_unlock(&done_mutex);
}

int setNodeColor(Node **nodeArray, Color colors[], Node *colorNode, Color color)
{
    int result = 1;
    int index = colorNode->getIndex();
    switch (colors[index])
    {
    case RED:
        if (color == RED)
        {
            return 0;
        }
        DEBUG("Trying to set an already colored node, something is wrong");
        throw colorex;
    case GREEN:
        if (color == GREEN)
        {
            return 0;
        }
        DEBUG("Trying to set an already colored node, something is wrong");
        throw colorex;
    case BLUE:
        if (color == BLUE)
        {
            return 0;
        }
        DEBUG("Trying to set an already colored node, something is wrong");
        throw colorex;
    default:
        colors[index] = color;
        break;
    }
    set<int>::iterator iter = nodeArray[index]->getVertices();
    while (iter != nodeArray[index]->getVerticesEnd())
    {
        int secondaryIndex = *iter;
        iter++;
        switch (color)
        {
        case RED:
            if (colors[secondaryIndex] == RED)
            {
                DEBUG("Red conflict from %d and %d.", index, secondaryIndex);
                throw colorex;
            }
            if (colors[secondaryIndex] == GREEN || colors[secondaryIndex] == BLUE)
            {
                break;
            }
            colors[secondaryIndex] = (Color)(NOT_RED | colors[secondaryIndex]);
            break;
        case GREEN:
            if (colors[secondaryIndex] == GREEN)
            {
                DEBUG("Green conflict from %d and %d.", index, secondaryIndex);
                throw colorex;
            }
            if (colors[secondaryIndex] == RED || colors[secondaryIndex] == BLUE)
            {
                break;
            }
            colors[secondaryIndex] = (Color)(NOT_GREEN | colors[secondaryIndex]);
            break;
        case BLUE:
            if (colors[secondaryIndex] == BLUE)
            {
                DEBUG("Blue conflict from %d and %d.", index, secondaryIndex);
                throw colorex;
            }
            if (colors[secondaryIndex] == RED || colors[secondaryIndex] == GREEN)
            {
                break;
            }
            colors[secondaryIndex] = (Color)(NOT_BLUE | colors[secondaryIndex]);
            break;
    		default:
           	break;
        }
        switch (colors[secondaryIndex])
        {
        case NOT_RED_OR_GREEN:
            result += setNodeColor(nodeArray, colors, nodeArray[secondaryIndex], BLUE);
            break;
        case NOT_RED_OR_BLUE:
            result += setNodeColor(nodeArray, colors, nodeArray[secondaryIndex], GREEN);
            break;
        case NOT_GREEN_OR_BLUE:
            result += setNodeColor(nodeArray, colors, nodeArray[secondaryIndex], RED);
            break;
        case IMPOSSIBLE:
            DEBUG("Determined an impossible coloring.");
            throw colorex;
        default:
            break;
        }
    }
    return result;
}

void * taskCliqueCheck(void *arg_in)
{
    Node **nodeArray = (Node **)arg_in;
    int last_state, last_type;
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);
    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &last_type);

    for (unsigned int aindex = 0; aindex + 3 < g_count; aindex++)
    {
        pthread_testcancel();
        if (nodeArray[aindex]->getVertexCount() < 3)
        {
            // This node does not have enough vertices to be a 4-clique
            continue;
        }
        set<int>::iterator b = nodeArray[aindex]->getVerticesAtOrAfter(aindex + 1);
        while (b != nodeArray[aindex]->getVerticesEnd())
        {
            int bindex = *b;
            b++;
            // Would be nice to skip the last 2 elements but the iterator has no "remaining" access.
            set<int>::iterator c = nodeArray[bindex]->getVerticesAtOrAfter(bindex + 1);
            while (c != nodeArray[bindex]->getVerticesEnd())
            {
                int cindex = *c;
                c++;
                if (! nodeArray[aindex]->hasVertex(cindex))
                {
                    continue;
                }
                set<int>::iterator d = nodeArray[cindex]->getVerticesAtOrAfter(cindex + 1);
                while (d != nodeArray[cindex]->getVerticesEnd())
                {
                    int dindex = *d;
                    d++;
                    if (! nodeArray[aindex]->hasVertex(dindex))
                    {
                        continue;
                    }
                    if (! nodeArray[bindex]->hasVertex(dindex))
                    {
                        continue;
                    }
                    DEBUG("Task one returning fail: %s, %s, %s, %s.", 
                          nodeArray[aindex]->getName().c_str(), 
                          nodeArray[bindex]->getName().c_str(), 
                          nodeArray[cindex]->getName().c_str(), 
                          nodeArray[dindex]->getName().c_str());
                    setSolution(NOT_COLORABLE, NULL);
                    return ((void *) &FAILED_CLIQUE_CHECK);
                }
            }
        }
    }
    DEBUG("Task one returning success");
    // Not calling conditional because we are not "done".
    // We failed to get an actual answer.
    return ((void *) &SUCCESS);
}

struct greaterNode
{
  bool operator()(Node* n1, Node* n2) const
  {
    if (n1->getVertexCount() > n2->getVertexCount())
    {
      return true;
    }
    if (n1->getVertexCount() < n2->getVertexCount())
    {
      return false;
    }
    return strcmp(n1->getName().c_str(), n2->getName().c_str()) < 0;
  }
};


int getGreedyCandidate(Color* colorTest, bool *forest, set<Node*, greaterNode> twoCandidateNodes, set<Node*, greaterNode> threeCandidateNodes)
{
/*
    if (! twoCandidateNodes.empty())
    {
        set<Node*, greaterNode>::iterator begin = twoCandidateNodes.begin();
        Node *first = *begin;
        return first->getIndex();
    }
    if (! threeCandidateNodes.empty())
    {
        // We have found a forest
        set<Node*, greaterNode>::iterator begin = threeCandidateNodes.begin();
        Node *first = *begin;
        return first->getIndex();
    }
    // Check that all the nodes are colored.
    for (unsigned int cntr = 0; cntr < g_count; cntr++)
    {
        switch(colorTest[cntr])
        {
        case RED:
        case GREEN:
        case BLUE:
            break;
        case IMPOSSIBLE:
            return -2;
        default:
            throw colorex;
        }
    }
    return -1;
*/
    int candidate = -1;
    for (unsigned int cntr = 0; cntr < g_count; cntr++)
    {
        switch(colorTest[cntr])
        {
        case UNKNOWN:
            if (candidate < 0)
            {
                candidate = cntr;
            }
            break;
        case RED:
        case GREEN:
        case BLUE:
            break;
        case NOT_RED:
        case NOT_GREEN:
        case NOT_BLUE:
            return cntr;
        case NOT_RED_OR_GREEN:
        case NOT_RED_OR_BLUE:
        case NOT_GREEN_OR_BLUE:
            DEBUG("Could not get a coloring in getGreedyCandidate");
            throw colorex;
        case IMPOSSIBLE:
            return -2;
        default:
            DEBUG("Got an unknown coloring in getGreedyCandidate");
            throw colorex;
        }
    }
    // If candidate >=0, then we are dealing with a forest;
    if (candidate >= 0)
    {
        *forest = true;
        DEBUG("We are dealing with a forest!");
    }
    return candidate;
}

Color getColorCandidate(Color current, Color attempt)
{
    switch (attempt)
    {
    case UNKNOWN:
        if (current == NOT_RED)
        {
            return GREEN;
        }
        else
        {
            return RED;
        }
    case RED:
        if (current == NOT_GREEN)
        {
            return BLUE;
        }
        else
        {
            return GREEN;
        }
    case GREEN:
        if (current == NOT_BLUE)
        {
            return IMPOSSIBLE;
        }
        else
        {
            return BLUE;
        }
    case BLUE:
        return IMPOSSIBLE;
    default:
        DEBUG("Could an unknown coloring %d in getColorCandidate", attempt);
        throw colorex;
    }
}

bool isColored(Color test)
{
    switch(test)
    {
    case RED:
    case GREEN:
    case BLUE:
        return true;
    default:
        return false;
    }
}

Color negativeColor(Color positive)
{
	  switch(positive)
    {
    case RED:
        return NOT_RED;
    case GREEN:
        return NOT_GREEN;
    case BLUE:
        return NOT_BLUE;
    default:
        DEBUG("Could an unknown coloring in negativeColor");
        throw colorex;
    }
}

Color impliedColor(Color negative)
{
    switch(negative)
    {
    case NOT_RED_OR_GREEN:
        return BLUE;
    case NOT_RED_OR_BLUE:
        return GREEN;
    case NOT_GREEN_OR_BLUE:
				return RED;
    default:
        return negative;
    }
}

struct GreedyState
{
    int candidate;
    Color current;
    Color attempt;
    vector<int> nodesUpdated;
    map<int, Color> revertMap;
};

int getOptionsForColor(Color color)
{
    switch(color)
    {
    case UNKNOWN:
        return 3;
    case RED:
    case GREEN:
    case BLUE:
        return 1;
    case NOT_RED:
    case NOT_GREEN:
    case NOT_BLUE:
        return 2;
    case NOT_RED_OR_GREEN:
    case NOT_RED_OR_BLUE:
    case NOT_GREEN_OR_BLUE:
        return 1;
    case IMPOSSIBLE:
        DEBUG("Got an impossible coloring in getOptionsForColor");
        throw colorex;
    default:
        DEBUG("Got an unknown coloring in getOptionsForColor");
        throw colorex;
    }
}

void setNodeInOptionSet(set<Node*, greaterNode> twoOptionNodes, set<Node*, greaterNode> threeOptionNodes, Node *node, Color oldColor, Color newColor)
{
/*
    int oldOptions = getOptionsForColor(oldColor);
    if (oldOptions == 2)
    {
        twoOptionNodes.erase(node);
    }
    if (oldOptions == 3)
    {
        threeOptionNodes.erase(node);
    }
    int newOptions = getOptionsForColor(newColor);
    if (newOptions == 2)
    {
        twoOptionNodes.insert(node);
    }
    else if (newOptions == 3)
    {
        threeOptionNodes.insert(node);
    }
*/
}

void printColorTest(Node **nodeArray, Color* colorTest)
{
    cout << "Printing the current color test" << endl;
    for (unsigned int cntr = 0; cntr < g_count; cntr++)
    {
        cout << nodeArray[cntr]->getName() << ": " << colorTest[cntr] << endl;
    }
}

Solution updateGreedyNodes(GreedyState* state, Node** nodeArray, Color* colorTest, set<Node*, greaterNode> twoOptionNodes, set<Node*, greaterNode> threeOptionNodes)
{
    for (unsigned int cntr = 0; cntr < state->nodesUpdated.size(); cntr++)
    {
        int updated = state->nodesUpdated[cntr];
        Color updatedColor = colorTest[updated];

        // compute coloring implications and record any change we made for the coloring attempt
        set<int>::iterator iter = nodeArray[updated]->getVertices();
        while (iter != nodeArray[updated]->getVerticesEnd())
        {
            int effected = *iter;
            iter++;
            if (isColored(colorTest[effected]))
            {
                if (colorTest[effected] == updatedColor)
                {
                    // We have a collision, time to unwind and try again.
                    DEBUG("Got a color(%d) collision between nodes %s and %s.", updatedColor,
                          nodeArray[updated]->getName().c_str(), nodeArray[effected]->getName().c_str());
                    return NOT_COLORABLE;
                }
                continue;
            }
            Color before = colorTest[effected];
            Color after = (Color)(before | negativeColor(updatedColor));
            if (after == IMPOSSIBLE)
            {
                // TODO: Add clean up code.
                DEBUG("Got an impossible on node %s.", nodeArray[effected]->getName().c_str());
                return NOT_COLORABLE;
            }
            after = impliedColor(after);
            setNodeInOptionSet(twoOptionNodes, threeOptionNodes, nodeArray[effected], before, after);
            colorTest[effected] = after;

            if (isColored(after))
            {
                // We just pushed a new node to colored so we need handle all of its subsidiary nodes.
                state->nodesUpdated.push_back(effected);
            }
            state->revertMap[effected] = before;
        }
    }
    return UNKNOWN_SOLUTION;
}

Solution greedySearch(Node **nodeArray, Color* colorTest)
{
    vector<GreedyState*> stateVector;
    set<Node*, greaterNode> twoOptionNodes;
    set<Node*, greaterNode> threeOptionNodes;
    // push all the nodes in to the correct options sets.
/*
    for (unsigned int cntr = 0; cntr < g_count; cntr++)
    {
        Node* optionNode = nodeArray[cntr];
        int options = getOptionsForColor(colorTest[cntr]);
        if (options == 3)
        {
        		threeOptionNodes.insert(optionNode);
        }
        else if (options == 2)
        {
        		twoOptionNodes.insert(optionNode);
        }
    }
*/
    size_t vectorCapacity = stateVector.capacity();
    if (vectorCapacity + 2 < g_count)
    {
       stateVector.reserve(g_count - 2);
    }
    GreedyState* state = new GreedyState();
    state->candidate = -1;
    state->current = UNKNOWN;
    state->attempt = UNKNOWN;
    stateVector.push_back(state);

    while (! stateVector.empty())
    {
        // printColorTest(nodeArray, colorTest);
        pthread_testcancel();
        GreedyState* state = stateVector.back();
        // Find the next candidate to switch
        // Try to find a not-color node otherwise get an any color node (implies a forest - could optimize)
        bool forest = false;
        if (state->candidate == -1)
        {
            state->candidate = getGreedyCandidate(colorTest, &forest, twoOptionNodes, threeOptionNodes);
            state->current = colorTest[state->candidate];
        }
        else
        {
            // Undo our effects from last time and try again
            DEBUG("Having to try another path for %s!", nodeArray[state->candidate]->getName().c_str());
            map<int, Color>::iterator revertIter;
            revertIter = state->revertMap.begin();
            while (revertIter != state->revertMap.end())
            {
                int index = revertIter->first;
                Color current = colorTest[index];
                Color revertColor = revertIter->second;
                setNodeInOptionSet(twoOptionNodes, threeOptionNodes, nodeArray[index], current, revertColor);
                colorTest[index] = revertColor;
                revertIter++;
            }
            colorTest[state->candidate] = state->current;
            state->revertMap.clear();
            state->nodesUpdated.clear();
        }

        // Check to see if we are done (solveable or not) return the solution
        if (state->candidate == -1)
        {
            // TODO: Add clean up code.
            // All indexes are colored we have a solution
            return SOLVEABLE;
        }
        if (state->candidate == -2)
        {
            // We failed to find a solution on this path.
            // Time to backtrack and try again.
            // revert the local change and try the next possible color
            DEBUG("Having to do a pre-backtrack!");
            map<int, Color>::iterator revertIter;
            revertIter = state->revertMap.begin();
            while (revertIter != state->revertMap.end())
            {
                int index = revertIter->first;
                Color current = colorTest[index];
                Color revertColor = revertIter->second;
                setNodeInOptionSet(twoOptionNodes, threeOptionNodes, nodeArray[index], current, revertColor);
                colorTest[index] = revertColor;
                revertIter++;
            }
            colorTest[state->candidate] = state->current;

            delete state;
            stateVector.pop_back();
            continue;
        }

        // determine a list of possible changes and try them each
        DEBUG("Calling getColorCandidate on %s with %d and %d.", nodeArray[state->candidate]->getName().c_str(), state->current, state->attempt);
        state->attempt = getColorCandidate(state->current, state->attempt);
        if (state->attempt == IMPOSSIBLE)
        {
            // We failed to find a solution on this path.
            // Time to backtrack and try again.
            // revert the local change and try the next possible color
            DEBUG("Having to do a pre-backtrack node %s failed after %d!", 
                  nodeArray[state->candidate]->getName().c_str(), state->attempt);
            map<int, Color>::iterator revertIter;
            revertIter = state->revertMap.begin();
            while (revertIter != state->revertMap.end())
            {
                int index = revertIter->first;
                Color current = colorTest[index];
                Color revertColor = revertIter->second;
                setNodeInOptionSet(twoOptionNodes, threeOptionNodes, nodeArray[index], current, revertColor);
                colorTest[index] = revertColor;
                revertIter++;
            }
            colorTest[state->candidate] = state->current;

            delete state;
            stateVector.pop_back();
            continue;
        }
        colorTest[state->candidate] = state->attempt;
        setNodeInOptionSet(twoOptionNodes, threeOptionNodes, nodeArray[state->candidate], state->current, state->attempt);

        state->nodesUpdated.push_back(state->candidate);

        Solution updateStatus = updateGreedyNodes(state, nodeArray, colorTest, twoOptionNodes, threeOptionNodes);
        if (updateStatus == NOT_COLORABLE)
        {
            DEBUG("Backtrack when trying to set %s to %d.", nodeArray[state->candidate]->getName().c_str(), state->attempt);
            // We failed to find a solution on this path.
            // Time to backtrack and try again.
            // revert the local change and try the next possible color
            DEBUG("Having to do a post-backtrack on setting %s from %d to %d!", 
                  nodeArray[state->candidate]->getName().c_str(), state->attempt, state->current);
            map<int, Color>::iterator revertIter;
            revertIter = state->revertMap.begin();
            while (revertIter != state->revertMap.end())
            {
                int index = revertIter->first;
                Color current = colorTest[index];
                Color revertColor = revertIter->second;
                setNodeInOptionSet(twoOptionNodes, threeOptionNodes, nodeArray[index], current, revertColor);
                colorTest[index] = revertColor;
                DEBUG("Reverting %s from %d to %d.", nodeArray[index]->getName().c_str(), current, revertColor);
                revertIter++;
            }
            colorTest[state->candidate] = state->current;

            delete state;
            stateVector.pop_back();
            continue;
        }

        // We have computed all the effected nodes.
        // Time to start looking at the next node, which means creating a new state element.
        state = new GreedyState();
        state->candidate = -1;
        state->current = UNKNOWN;
        state->attempt = UNKNOWN;
        stateVector.push_back(state);
    }

    // if no solution is found return NOT_COLORABLE
    return NOT_COLORABLE;
}

void * taskGreedyThreeClique(void *arg_in)
{
    Node **nodeArray = (Node **)arg_in;
    int last_state, last_type;
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);
    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &last_type);
    vector<ThreeClique> threeCliqueVector;
    DEBUG("Task two");

    // Determine if we have any 3 cliques
    // If we do start with the three clique with the highest vertex count.
    // Otherwise pick the single node with the highest vertex count.
    for (unsigned int aindex = 0; aindex + 3 < g_count; aindex++)
    {
        pthread_testcancel();
        if (nodeArray[aindex]->getVertexCount() < 3)
        {
            // This node does not have enough vertices to be a 4-clique
            continue;
        }
        set<int>::iterator b = nodeArray[aindex]->getVerticesAtOrAfter(aindex + 1);
        while (b != nodeArray[aindex]->getVerticesEnd())
        {
            int bindex = *b;
            b++;
            // Would be nice to skip the last 2 elements but the iterator has no "remaining" access.
            set<int>::iterator c = nodeArray[bindex]->getVerticesAtOrAfter(bindex + 1);
            while (c != nodeArray[bindex]->getVerticesEnd())
            {
                int cindex = *c;
                c++;
                if (! nodeArray[aindex]->hasVertex(cindex))
                {
                    continue;
                }
                // We have found a 3-clique add it to the list.
                threeCliqueVector.push_back(ThreeClique(nodeArray[aindex], nodeArray[bindex], nodeArray[cindex]));
                DEBUG(threeCliqueVector[threeCliqueVector.size() - 1].print().c_str());
            }
        }
    }

    if (threeCliqueVector.size() > 0)
    {
        DEBUG("We have %ld three-cliques, going greedy that way.", threeCliqueVector.size());
        // Determine the 3-count with the highest vertex count and start there.
        ThreeClique greediestClique = threeCliqueVector[0];
        unsigned int cntr;
        for (cntr = 1; cntr < threeCliqueVector.size(); cntr++)
        {
            pthread_testcancel();
            ThreeClique testClique = threeCliqueVector[cntr];
            if (greediestClique.getVertexCount() < testClique.getVertexCount())
            {
                DEBUG(testClique.print().c_str());
                greediestClique = testClique;
            }
        }
        DEBUG(greediestClique.print().c_str());

        // Start marking all the nodes with the appropriate colors.
        Color colors[g_count];
        for (cntr = 0; cntr < g_count; cntr++)
        {
            colors[cntr] = UNKNOWN;
        }
        unsigned int colored = 0;
        try
        {
           colored += setNodeColor(nodeArray, colors, greediestClique.getNodeA(), RED);
           colored += setNodeColor(nodeArray, colors, greediestClique.getNodeB(), GREEN);
           colored += setNodeColor(nodeArray, colors, greediestClique.getNodeC(), BLUE);
        }
        catch (colorexception e)
        {
            DEBUG("Got an exception coloring the graph, must be unsolveable.");
            setSolution(NOT_COLORABLE, NULL);

            return ((void *) &SUCCESS);
        }
        if (colored >= g_count)
        {
            DEBUG("We have colored %d nodes.", colored);

            // We were able to do a straight compute of the colors - return the result.
            setSolution(SOLVEABLE, colors);

            return ((void *) &SUCCESS);
        }

        // Could not do a straight compute of the answer - Time to get greedy
        Color colorTest[g_count];
        for (cntr = 0; cntr < g_count; cntr++)
        {
            colorTest[cntr] = colors[cntr];
        }
        Solution greedySolution = greedySearch(nodeArray, colorTest);
        if (greedySolution == SOLVEABLE)
        {
            DEBUG("Greedy search found the problem to be solveable.");
            setSolution(SOLVEABLE, colorTest);
        }
        else
        {
            DEBUG("Greedy search returned %d.", greedySolution);
            setSolution(NOT_COLORABLE, NULL);
        }
    }

    // If we found no 3-cliques that proves nothing.
    return ((void *) &SUCCESS);
}

void * taskGreedySearch(void *arg_in)
{
    Node **nodeArray = (Node **)arg_in;
    int last_state, last_type;
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);
    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, &last_type);
    DEBUG("Task three");

    DEBUG("Trying a simple greedy algorithm, no 3-clique optimization.");
    // Find the node with the highest vertex count and mark it RED
    int candidate = 0;
    for (unsigned int cntr = 1; cntr < g_count; cntr++)
    {
        if (nodeArray[candidate]->getVertexCount() < nodeArray[cntr]->getVertexCount())
        {
            candidate = cntr;
        }
    }
    // Find the node next to it with the highest vertex cound and mark it GREEN
    set<int>::iterator iter = nodeArray[candidate]->getVertices();
    int second = -1;
    while (iter != nodeArray[candidate]->getVerticesEnd())
    {
        int test = *iter;
        iter++;
        if (second == -1 || nodeArray[second]->getVertexCount() < nodeArray[test]->getVertexCount())
        {
            second = test;
				}
    }
    // Start marking all the nodes with the appropriate colors.
    Color colors[g_count];
    for (unsigned int cntr = 0; cntr < g_count; cntr++)
    {
        colors[cntr] = UNKNOWN;
    }
    int colored = 0;
    try
    {
       colored += setNodeColor(nodeArray, colors, nodeArray[candidate], RED);
       colored += setNodeColor(nodeArray, colors, nodeArray[second], GREEN);
    }
    catch (colorexception e)
    {
        DEBUG("Got an exception coloring the graph, must be unsolveable.");
        setSolution(NOT_COLORABLE, NULL);

        return ((void *) &SUCCESS);
    }
    
    // Could not do a straight compute of the answer - Time to get greedy
    Color colorTest[g_count];
    for (unsigned int cntr = 0; cntr < g_count; cntr++)
    {
        colorTest[cntr] = colors[cntr];
    }
    Solution greedySolution = greedySearch(nodeArray, colorTest);
    if (greedySolution == SOLVEABLE)
    {
        DEBUG("Greedy search found the problem to be solveable.");
        setSolution(SOLVEABLE, colorTest);
    }
    else
    {
        DEBUG("Greedy search returned %d.", greedySolution);
        setSolution(NOT_COLORABLE, NULL);
    }

    return ((void *) &SUCCESS);
}

string trimDirectory(string path)
{
    return path.substr( path.find_last_of( "/\\" ) + 1);
}

void printSolution(char* program, char* input, Solution solution, Node **nodeArray)
{
    ofstream resultFile;
    string programName = trimDirectory(program);
    string inputName = trimDirectory(input);
    // stringstream fileName;
    // fileName << "" << programName << "_" << inputName << "_out";
    // DEBUG("Outputting to file %s.", fileName.str());
    // resultFile.open(fileName.str().c_str());
    char fileName[512];
    sprintf(fileName, "%s_%s_out", programName.c_str(), inputName.c_str());
    resultFile.open(fileName);

    pthread_mutex_lock(&done_mutex);
    if (solution == SOLVEABLE)
    {
        cout << "true" << endl;
        resultFile << "true" << endl;
        for (unsigned int cntr = 0; cntr < g_count; cntr++)
        {
            cout << nodeArray[cntr]->getName() << ": " << fixColor(colorSolution[cntr]) << "\n";
            resultFile << nodeArray[cntr]->getName() << ": " << fixColor(colorSolution[cntr]) << "\n";
        }
    }
    else
    {
        cout << "false" << endl;
        resultFile << "false" << endl;
        DEBUG("Solution is set to %d.", solution);
    }
    pthread_mutex_unlock(&done_mutex);
    resultFile.close();
    DEBUG("Done");
}

int main(int argc, char **argv)
{
    TIMER("START");
    ifstream inFile;
    inFile.open(argv[1]);
    int numNodes;
    string s;
    Node **nodeArray;

    int *statusOne;
    size_t default_stack_size;
    pthread_attr_t stack_size_custom_attr;
    pthread_attr_init(&stack_size_custom_attr);
    pthread_attr_getstacksize(&stack_size_custom_attr, &default_stack_size);
    pthread_t thread1, thread2, thread3;

    TIMER("BEGIN_FILE_READ");
    if (!inFile)
    {
        cout << "Unable to open file " << argv[1] << endl;
        exit(1); // terminate with error
    }
    inFile >> numNodes;
    if (numNodes <= 0)
    {
        cout << "true" << endl;
        exit(1);
    }
    getline(inFile, s);
    nodeArray = new Node*[numNodes];
    DEBUG("We have %d nodes.\n\n", numNodes);
    g_count = numNodes;
    colorSolution = new Color[g_count];

    // Origanally assumed that the 27th element would be "AA".
    // This allowed for an efficient 1-pass loading of data.
    // However we now need to be able to handle arbitrary ordering of data.
    // That implies we need to do a 2-pass load of data.
    int index = 0;
    hash_map<const char*, int, hash<const char*>, eqstr> nameIndexMap;
    nameIndexMap.resize(2 * g_count);

    while (getline(inFile, s))
    {
        size_t endpos = s.find_last_not_of(" \r\n");
        if (string::npos != endpos)
        {
            s = s.substr(0, endpos + 1);
        }
        Node *holder = new Node(s, index);
        DEBUG(holder->print().c_str());
        // int index = getIndexForStringName(holder->getName());
        // if (index > numNodes)
        // {
        //      cout << "We got a node with an index out of range!" << endl;
        //      exit(1);
        // }
        nodeArray[index] = holder;
        nameIndexMap[holder->getName().c_str()] = index++;
    }
    TIMER("END_FILE_READ");

    for (int cntr = 0; cntr < numNodes; cntr++)
    {
        nodeArray[cntr]->determineVertices(nameIndexMap);
        // DEBUG(nodeArray[cntr]->print().c_str());
    }
    TIMER("END_SECOND_PASS");

    pthread_create(&thread1, NULL, taskCliqueCheck, nodeArray);
    pthread_create(&thread2, NULL, taskGreedyThreeClique, nodeArray);
    pthread_create(&thread3, NULL, taskGreedySearch, nodeArray);
    TIMER("END_THREAD_CREATION");

    pthread_mutex_lock(&done_mutex);
    while (solution == UNKNOWN_SOLUTION)
    {
        pthread_cond_wait(&done_cond, &done_mutex);
    }
    pthread_mutex_unlock(&done_mutex);
    TIMER("END_WAIT");

    // tell all the threads to clean up and end, we are done here.
    pthread_cancel(thread1);
    pthread_cancel(thread2);
    pthread_cancel(thread3);
    TIMER("END_THREAD_CANCEL");

    pthread_join(thread1, (void **)&statusOne);
    pthread_join(thread2, NULL);
    pthread_join(thread3, NULL);
    TIMER("END_THREAD_JOIN");

    printSolution(argv[0], argv[1], solution, nodeArray);
    TIMER("END_PRINT_SOLUTION");

    // clean up
    for (int cntr = 0; cntr < numNodes; cntr++)
    {
        if (nodeArray[cntr] != NULL)
        {
            delete nodeArray[cntr];
            nodeArray[cntr] = NULL;
        }
        else
        {
            DEBUG("Not deleting node at %d.", cntr);
        }
    }
    delete [] nodeArray;
    TIMER("END_CLEAN_UP");
}
