# Bayesian Probabilistic Forecast of MLB Games

This directory contains:

* `Report.html`, the project report
* `Report.Rmd`, the RMD code generating the report
* `GL2018.csv`, the game logs for 2018 MLB games from Retrosheet
* `2019thru0416.csv`, line scores for 2019 MLB games through Apr 16
* `RetrosheetToGameState.java`, the Java code used to produce game states from game logs and line scores
* `Code.R`, the R code used for data analysis on the game states
* `gamestates.csv`, the training set of game states generated from the 2018 games
* `teststates.csv`, the test set of game states generated from the 2019 games
* `Complete 2019-04-27.RData`, the R dataset produced by running `Code.R`
