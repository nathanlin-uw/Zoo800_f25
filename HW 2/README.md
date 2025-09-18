# Quantitative Methods in Ecology and Evolution, Homework 2
# Nathan Lin and Nurul Islam
# September 11, 2025
# Evaluating open science in our fields

Objective 1
Nathan: I searched Google Scholar using the search terms "ticks salivary glands expression ixodes".
- Of these 10, none of them had code available, almost all of them used a combination of open source and proprietary software, and almost all of them had data available through NCBI or a proteome database or available to download as supplementary material. We found one that said "The data that has been used is confidential."...
Nurul: I searched Google Scholar using the search terms "Chronic Wasting Disease white-tailed deer movement"
- Of these 10, 3 of them had both code and data available. For three of these, data was not publicly available (either held by DNR or not released due to something about deer being a protected species), and for another it said it was available only upon request. For the others, the data was available through dryad, github, and movebank. Code was available through Zenodo and Github. Almost all of these papers used R.

Objective 2
Only one paper had code and data on github (https://github.com/szandrapeter/human-mobility-animal-movement-integration.git).
We could run some of the code after installing a bunch of packages but some data file apparently just did not exist and upon checking the paper again they didn't say how they made their non-map figures (those were done on ArcGIS). 
We tried to run every single script available but somehow every single one errored (and it was like a "failed to create shared library 4 node produced errors" thing, not just path related errors), and no figures were generated from that.

Edit: Tried your 2023 paper but the fishsum function was not working for the 2018 and 2019 data. I then tried the Salmonid body size one and was able to get Figure 4 after changing the path to the data csv.
