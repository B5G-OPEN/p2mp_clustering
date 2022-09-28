# p2mp_clustering
A decorrelation based algorithm for P2MP clustering of network nodes

This code takes as input a number N of access nodes with either a business or residential traffic pattern and finds an optimal partitioning (based on clustering) of nodes into groups where the sum of the traffic per group never exceeds 16x25 Gb/s (i.e. 400 Gb/s total capacity) at any time of the day.
