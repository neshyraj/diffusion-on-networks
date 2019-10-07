# diffusion-on-networks

Completed March 27th 2019

Project at University of Oxford simulating contagion and network diffusion, where the adoption of a microfinance program is the "contagion" and the networks are connections within various small villages in India.

This project is based on The Diffusion of Microfinance and respective data by Banerjee et al. One adjacency matrix and household characteristcs included here, but full article and full data + source found at (DOI: 10.1126/science.1236498)
Looks at the community structure of social networks in Indian villages. Uses homophily via assortativity to try to establish some pattern of program adoption (contagion). Goal is then to devise a strategy to choose "leaders" within each village to maximize adoption rates. Strategy is simualted as a model of diffusion on the networks.

Project done as part of the class C5.10 Mathematics and Data Science for Development with Dr. Neave O'Clery

adjmat1 - adjacency matrix of one village. each row/column represents a household. all adjacency matrices found at link above.

calcs_clust - various calculations such as assortativty, mixing matrix, adoption etc. as well as Louvain and spectral clustering

datacom/datanew - manually generated files used for plotting community level adoption

diffusion - function to run diffusion model and function to calculate diffusion centrality

document - written report. first page is a policy brief and rest is an article in PNAS format

householdcharacteristics - characteristics such as caste, religion, leader status etc. for each of the households across all villages

leaders - implement leader choosing strategy and train logistic regression models on chosen leaders' household characteristics

plot - various plots for analysis/visualization as well as linear regressions to see effect of attributes/characteristics on adoption at each diffusion timestep

process - import and prepare data for analysis by reformatting, creating networks and setting network attributes. heavily uses igraph.

processing and plot - imports data and converts to igraph objects for analysis and computations. does plotting for visualization

variationinfo - calculates variation of information after rewiring and plots to check for robustness of communities
