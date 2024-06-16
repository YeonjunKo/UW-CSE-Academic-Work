# valueIterationAgents.py
# -----------------------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
#
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


# valueIterationAgents.py
# -----------------------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
#
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


import mdp, util

from learningAgents import ValueEstimationAgent
import collections

class ValueIterationAgent(ValueEstimationAgent):
    """
        * Please read learningAgents.py before reading this.*

        A ValueIterationAgent takes a Markov decision process
        (see mdp.py) on initialization and runs value iteration
        for a given number of iterations using the supplied
        discount factor.
    """
    def __init__(self, mdp, discount = 0.9, iterations = 100):
        """
          Your value iteration agent should take an mdp on
          construction, run the indicated number of iterations
          and then act according to the resulting policy.

          Some useful mdp methods you will use:
              mdp.getStates()
              mdp.getPossibleActions(state)
              mdp.getTransitionStatesAndProbs(state, action)
              mdp.getReward(state, action, nextState)
              mdp.isTerminal(state)
        """
        self.mdp = mdp
        self.discount = discount
        self.iterations = iterations
        self.values = util.Counter() # A Counter is a dict with default 0
        self.runValueIteration()

    def runValueIteration(self):
        # Write value iteration code here
        for r in range(self.iterations):
            oldVal = self.values.copy()
            for s in self.mdp.getStates():

                if self.mdp.isTerminal(s):
                    continue

                maxQ = util.Counter()
                for a in self.mdp.getPossibleActions(s):
                    for ns, p in self.mdp.getTransitionStatesAndProbs(s, a):
                        maxQ[a] += p * (self.mdp.getReward(s, a, ns) + self.discount * oldVal[ns])

                self.values[s] = maxQ[maxQ.argMax()]

    def getValue(self, state):
        """
          Return the value of the state (computed in __init__).
        """
        return self.values[state]


    def computeQValueFromValues(self, state, action):
        """
          Compute the Q-value of action in state from the
          value function stored in self.values.
        """
        "*** YOUR CODE HERE ***"
        qVal = 0
        for ns, p in self.mdp.getTransitionStatesAndProbs(state, action):
            qVal += p * (self.mdp.getReward(state, action, ns) + self.discount * self.getValue(ns))

        return qVal

    def computeActionFromValues(self, state):
        """
          The policy is the best action in the given state
          according to the values currently stored in self.values.

          You may break ties any way you see fit.  Note that if
          there are no legal actions, which is the case at the
          terminal state, you should return None.
        """
        "*** YOUR CODE HERE ***"
        if self.mdp.isTerminal(state):
            return None

        compute = util.Counter()
        for action in self.mdp.getPossibleActions(state):
            compute[action] = self.getQValue(state, action)
        
        if compute.totalCount == 0:
            return None
        return compute.argMax()
        

    def getPolicy(self, state):
        return self.computeActionFromValues(state)

    def getAction(self, state):
        "Returns the policy at the state (no exploration)."
        return self.computeActionFromValues(state)

    def getQValue(self, state, action):
        return self.computeQValueFromValues(state, action)

class AsynchronousValueIterationAgent(ValueIterationAgent):
    """
        * Please read learningAgents.py before reading this.*

        An AsynchronousValueIterationAgent takes a Markov decision process
        (see mdp.py) on initialization and runs cyclic value iteration
        for a given number of iterations using the supplied
        discount factor.
    """
    def __init__(self, mdp, discount = 0.9, iterations = 1000):
        """
          Your cyclic value iteration agent should take an mdp on
          construction, run the indicated number of iterations,
          and then act according to the resulting policy. Each iteration
          updates the value of only one state, which cycles through
          the states list. If the chosen state is terminal, nothing
          happens in that iteration.

          Some useful mdp methods you will use:
              mdp.getStates()
              mdp.getPossibleActions(state)
              mdp.getTransitionStatesAndProbs(state, action)
              mdp.getReward(state)
              mdp.isTerminal(state)
        """
        ValueIterationAgent.__init__(self, mdp, discount, iterations)

    def runValueIteration(self):
        "*** YOUR CODE HERE ***"
        index = 0
        while index < self.iterations:
            for s in self.mdp.getStates():
                if index >= self.iterations:
                    return
                
                if self.mdp.isTerminal(s):
                    index += 1
                    continue

                maxQ = util.Counter()
                for a in self.mdp.getPossibleActions(s):
                    maxQ[a] += self.computeQValueFromValues(s, a) # use updated value right away
                self.values[s] = maxQ[maxQ.argMax()]

                index += 1

class PrioritizedSweepingValueIterationAgent(AsynchronousValueIterationAgent):
    """
        * Please read learningAgents.py before reading this.*

        A PrioritizedSweepingValueIterationAgent takes a Markov decision process
        (see mdp.py) on initialization and runs prioritized sweeping value iteration
        for a given number of iterations using the supplied parameters.
    """
    def __init__(self, mdp, discount = 0.9, iterations = 100, theta = 1e-5):
        """
          Your prioritized sweeping value iteration agent should take an mdp on
          construction, run the indicated number of iterations,
          and then act according to the resulting policy.
        """
        self.theta = theta
        ValueIterationAgent.__init__(self, mdp, discount, iterations)

    def runValueIteration(self):
        "*** YOUR CODE HERE ***"
        # compute predecessors of all states
        pred = util.Counter()
        for s in self.mdp.getStates():
            if self.mdp.isTerminal(s):
                continue

            for a in self.mdp.getPossibleActions(s):
                for ns, p in self.mdp.getTransitionStatesAndProbs(s, a):
                    if ns not in pred:
                        pred[ns] = set()
                        pred[ns].add(s)
                    else:
                        pred[ns].add(s)

        # initialize an empty priority queue
        pq = util.PriorityQueue()
        # For each non-terminal state s
        for s in self.mdp.getStates():
            if self.mdp.isTerminal(s):
                continue

            # Find absolute value of diff between current val of s in self.values
            # and the highest q val across all possible actions from s
            maxQ = float('-inf')
            for a in self.mdp.getPossibleActions(s):
                maxQ = max(maxQ, self.computeQValueFromValues(s, a))
            
            diff = abs(self.values[s] - maxQ)
            # push s into pq with -diff prio.
            pq.push(s, -diff)

        # For iteration in 0, 1, 2, ..., self.iterations - 1
        for r in range(self.iterations):
            # If pq is empty, terminate
            if pq.isEmpty():
                break
            
            # pop a state s off the pq
            cur_s = pq.pop()

            if not self.mdp.isTerminal(cur_s):
                # update s's value in self.values if not terminal state
                qVal = util.Counter()
                for a in self.mdp.getPossibleActions(cur_s):
                    qVal[a] += self.computeQValueFromValues(cur_s, a)
                self.values[cur_s] = qVal[qVal.argMax()]

                # For each predcessor p of s, do:
                for p in pred[cur_s]:
                    # Find absolute value of diff between current val of p in self.values
                    # and the highest q val across all possible actions from p
                    maxpredQ = float('-inf')
                    for act in self.mdp.getPossibleActions(p):
                        maxpredQ = max(maxpredQ, self.computeQValueFromValues(p, act))
                    diff = abs(self.values[p] - maxpredQ)

                    # if diff > theta, push p into pq with -diff prio.
                    if diff > self.theta:
                        pq.update(p, -diff)