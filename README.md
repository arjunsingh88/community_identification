# Social Network Analysis
Social network analysis is a way to find important nodes in a network and understand how the network interacts.

The term ‘social’ implies interactions among humans, but social network analysis can help us understand interactions between anything – from devices on an IT network to transactions between bank accounts.

### Topological measure 

|Local Measure | Global Measure |
|--------------|--------------|
|Degree <br> Eccentricity <br> Betweeness centrality <br> Closeness centrality <br> Edgebetweenness <br> Local transivity|Density <br> Diameter & Radius <br> Transivity <br> Modularity <br> Average|


|Centrality Measures|![1_fPuf75BXYYGC3VmhSXnb2w](https://user-images.githubusercontent.com/45566835/83137424-70e37380-a0e9-11ea-9f1c-7890866d521a.png)|
|--|--|
# Community Detection
`Community:` A dense subgraph loosely coupled to other modules in the network. A community is a set of nodes seen as one by nodes outside the community. A subgraph where almost all nodes are linked to other nodes in the community.

![community](https://user-images.githubusercontent.com/45566835/83134775-2102ad80-a0e5-11ea-8796-fe35a72efc65.png)

### Local Modularity Measures
`Modularity: `Modularity is one measure of the structure of networks or graphs. It was designed to measure the strength of division of a network into modules (also called groups, clusters or communities). Networks with high modularity have dense connections between the nodes within modules but sparse connections between nodes in different modules. Modularity is often used in optimization methods for detecting community structure in networks.
| Modularity Measure | Mathematical computation |
|:-------------:|:-------------:|
|      `Modularity R`         | ![lcoal community function](https://user-images.githubusercontent.com/45566835/83134771-206a1700-a0e5-11ea-8b1e-e7c0784fbfa2.png) |
|      `Modularity M`        |![2](https://user-images.githubusercontent.com/45566835/83134772-206a1700-a0e5-11ea-92da-297f85a156aa.png)      |
|      `Modularity L`         | ![3](https://user-images.githubusercontent.com/45566835/83134774-2102ad80-a0e5-11ea-92b0-6c58c9df7f0e.png) |

### Ground Truth Communities
![gt](https://user-images.githubusercontent.com/45566835/83136045-1812db80-a0e7-11ea-8aff-393a028b50eb.png)


## Model
* `Step 1.`	Selecting Networks for Ground truth(Karate, football, polbooks, dolphin)
* `Step 2.`	Computing the modularity values for different modularities(Mod_L, R, M ,S)
* `Step 3.`	Selecting the best modularity measure(Normalized) for every node, i.e one with max quality score (incase of tie to use voting method)
* `Step 4.`	compute centrality measures for the nodes(degree, betweness, closeness, eigen, subgraph, pagerank, infocent) and assign them modularity labels 
* `Step 5.`	Apply different ML supervised models (knn, randomforest, SVM)
* `Step 6.`	Compute the accuracy and compare result to select bes model for our case

