# social-networks-dissertation

Submitted April 29th, 2019

Masters dissertation at University of Oxford supervised by Dr. Francois Caron

Modeling and Analysis of Social Network Data from Rank Preferences. Given data in the form of ranked preferences within a social network, derived and implemented Expectation Maximization and Markov Chain Monte Carlo algorithms for maximum a posteriori parameter estimation on the affiliation levels between individuals in the network. Used estimates to cluster individuals into groups and recommend/predict further preferences. Took a Bayesian approach with the Plackett-Luce model.

dissertation - written dissertation, excluding appendix

em - code to run EM algorithm and to calculate log-likelihood

mcmc - code to run MCMC algorithm, find point estimates and plot autocorrelation and traceplots

pred - code to predict further preferences and sample distribution of recommended further preferences

sim - code to simulate data for model validation
