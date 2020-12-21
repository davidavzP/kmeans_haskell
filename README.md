# Kmeans Implemented in Haskell

This was a final project in my functional programming class. 
I implemented Kmeans from stratch using the inertial method.
Designed to cluster any data type in Haskell, with a defined distance metric.
This program requires the number of k clusters and n iterations to be specified.

Algorithm Steps:
- kmeans
  - Choose k centroids 
  - Assign data to k centroids using defined distance metric 
  - Calculate new centroids from mean distance
- Iterate n times 
  - Each iteration determines the best overall clustering variance from previous iterations 
  - Chooses the centroids for the next iteration from the previous best clustering

Results:

I tested this implementation on basic clustering data and found the iteration based variance improved performance with more iterations.

 


