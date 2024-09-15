### Instructions for replication of the analysis presented in
## Preventive War and Sovereign Debt <br><small> by Colin Krainin, Kristopher W. Ramsay, Bella Wang, and Joseph J. Ruggiero</small>


### I. Setup
Place all files in the same working directory.

### II. Packages
All packages are available through CRAN with ```install.packages```.

The following is a list of all R packages used in the making of this paper. 
Not all of them are required for the main replication.
- ```DAMisc```  
- ```dplyr```  
- ```extrafont```
- ```foreign```
- ```ggplot2```
- ```ggrepel```
- ```gridExtra```
- ```knitr```
- ```latex2exp```
- ```mfx```
- ```stargazer```
- ```texreg```
- ```tidyr```
- ```tidyverse```
- ```Zelig```

### III. Data

The following files contain all of the data used in the analysis.

* ```preventive cases.dta```

    This file was provided directly by [Douglas Lemke](https://sites.psu.edu/territorialcontenders/) in December 2020 and contains data used for analysis in the following paper.

    > Lemke, Douglas. 2003. "Investigating the preventive motive for war." *International Interactions* 29 (4): 273-292.

    It contains data from the Correlates of War project's COW dataset, as well as the preventive motive estimates from Lemke's paper.

* ```BellJohnsonISQrepDDyadic.dta```

    This file is omitted here due to size and can be retrieved from [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/MYBQTZ). It contains data used for analysis in the following paper.

    > Bell, Sam R. and Jesse C. Johnson. 2015. "Shifting Power, Commitment Problems, and Preventive War." <em>International Studies Quarterly</em> 59 (1): 133-144.

* ```LTCYUK.csv```

    This file contains data on the Consol yields in the United Kingdom (1753-2016). The data was collected by the [Bank of England's Three Centuries of Macroeconomic Data](https://www.bankofengland.co.uk/statistics/research-datasets) project and this file was retrieved from the [St. Louis Fed's Federal Reserve Economic Database (FRED)](https://fred.stlouisfed.org/series/LTCYUK) in December 2020.

* ```a-millennium-of-macroeconomic-data-for-the-uk.csv```

    This dataset contains a broad set of macroeconomic and financial data for the United Kingdom provided by the Bank of England. This file was retrieved from the [Bank of England research datasets](https://www.bankofengland.co.uk/statistics/research-datasets) in early 2021.

* ```wardebt.csv```

    This file contains the merged data that was used to run the regressions in the analysis. Note that ```wardebt-names.csv``` is the same file with several war names included for the figures.

To replicate our results, load the packages and create a dataframe from ```wardebt.csv```.
Alternatively, you can use the raw files and merge the relevant information.

### IV. Regressions

* ```wardebt-regs.R```

    This file contains the code that was used to run the regressions presented in the main text.

    This includes six models of logistic regressions on war using the preventive motive estimates from Lemke (2003) and presented in Table 2 of the main text, as well as two models of logistic regressions on war using the preventive motive estimate from Bell and Johnson (2015) and presented in Table 3 of the main text.

    It also calculates average marginal effects on the same tables.
    Click [here](https://cran.r-project.org/web/packages/mfx/mfx.pdf) for documentation about the ```logitmfx``` function.
    
    It also calculates the interaction effects for the first model run, which are plotted in the main text.
    Click [here](https://cran.r-project.org/web/packages/DAMisc/DAMisc.pdf) for documentation about the ```intEff``` function.
    See the following paper for more information about interaction effects and their differences from marginal effects.

    > Ai, Chunrong and Edward C. Norton. 2003. "Interaction terms in logit and probit models." <em>Economic Letters</em> 80 (1): 123-129.

* ```wardebt-supp.R```

    This file contains the code that was used to run the supplementary regressions presented outside of the main text. This includes but is not limited to replications of relevant regressions from the following two papers.

    > Lemke, Douglas. 2003. "Investigating the preventive motive for war." *International Interactions* 29 (4): 273-292.

    >Bell, Sam R. and Jesse C. Johnson. 2015. "Shifting Power, Commitment Problems, and Preventive War." <em>International Studies Quarterly</em> 59 (1): 133-144.

* ```logitmfxRE.R```, ```logitmfxestRE.R```, and ```secalc.R```

    These files contain necessary functions for executing the replication code.

The results for normal logistic and linear regressions were displayed using ```stargazer``` and the results for rare events logits were displayed using ```texreg```.

### V. Figures

* ```figs.R``` and ```plot.R```

    These files contains code to recreate the figures presented in the main text.

