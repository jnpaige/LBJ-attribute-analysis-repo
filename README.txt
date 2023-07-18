This directory is split into several datasets and R scripts.
LBJ attribute data.xlsx is the output of the original microsoft access database stored on JP's laptop.

Each distinct kind of analysis is done with a separate R script. Each script loads in the raw data, and calls a couple different functions to clean the data, and add more columns:

Functions that modify data
Clean Data.R - assigns each artifact to a level based on the reported depth, and sorts the levels in appropriate order. 
	     - Makes sure all the numeric data are numeric, and characters are characters.
	     - Adds a context column (just the site and test unit combined into one string)
	     - Adds an areabymass column, which is the flake area, divided by mass. This is intended to get at efficiency of raw material use. 

Convex hull function.R - Creates a function that makes it easier to draw a polygon around 2d data in ggplot2. 

Munsell color quantifier.R - Converts Munsell data into spatial coordinates. 


Analysis scripts:

Color analysis LBJ.R -Focused on assessing variability in color between sites, between components, and between raw materials collected during surveys in Central Texas and surroundin area

Cutting edge analysis.R - Measuring variability in flaking efficiency between sites, and components.


