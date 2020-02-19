# Semirings
This is an implementation of the paper ["Fun with Semirings"](http://stedolan.net/research/semirings.pdf). This paper introduces the mathematical concept of a semiring which, when equipped with an appropriate closure operation, can be used to solve several optimisation problems (reachability, shortest/longest/widest distance/path, knapsack etc.).

This repository uses the above concepts to solve some problems from programming competitions. The problems are in the directory src/Examples. Each example comes in its own sub-folder, which comprises of a solution, a PDF with the exercise, input files and output results.

Note that, when considering the shortest path problem, the closure operation is similar to the Floyd-Warshall algorithm in the sense that it calculates the shortest path between all pairs of nodes in a graph. So for Dijkstra/BFS style problems (e.g. Sabotage, Knights of Ni) this method is an overkill and runs out of memory for large test cases.