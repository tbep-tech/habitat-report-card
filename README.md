# habitat-report-card

Materials for creating a habitat report card for the Habitat Master Plan and Habitat Restoration Consortium

Report card: [link](https://drive.google.com/file/d/1Y9pC6CjPYlTYBMo_mOdgY5JLDa8JMImB/view)

Tables and figures: [link](https://tbep-tech.github.io/habitat-report-card/summaries.html)

Restoration potential map: [link](https://tbep-tech.github.io/habitat-report-card/restpotential.html)

## Updatings tables and figures

The tables and figures link above can be updated as source material for the habitat report card. This should be done each year once the file `restoration.csv` is updated in the [TBEP_Habitat_Restoration](https://github.com/tbep-tech/TBEP_Habitat_Restoration) repository. Follow the steps below to update the tables and figures.  These files depend on many R packages, so you will likely need to install them before running.

1. Run all of the contents in the file [`R/dat_proc.R](https://github.com/tbep-tech/habitat-report-card/blob/main/R/dat_proc.R) down to [line 45](https://github.com/tbep-tech/habitat-report-card/blob/623ce63c57a2f765752fab3cec4cc99cf3cd67d2/R/dat_proc.R#L45).  This retrieves the files `restoration.csv`, formats it, and saves it as an updated file called `rstdatall.RData` for creating the tables and figures.
1. Run all of the contents in the file [`R/figs.R`](https://github.com/tbep-tech/habitat-report-card/blob/main/R/figs.R).  This creates multiple figures that are placed in [`docs/figs`](https://github.com/tbep-tech/habitat-report-card/tree/main/docs/figs), which are used in the rendered Quarto file and can also be used as standalone files. 
1. Run all of the contents in the file [`R/tabs.R`](https://github.com/tbep-tech/habitat-report-card/blob/main/R/tabs.R).  This creates multiple tables that are placed in [`docs/tabs`](https://github.com/tbep-tech/habitat-report-card/tree/main/docs/tabs), which are used in the rendered Quarto file and can also be used as standalone files.
1. Open and render the Quarto file [`docs/summaries.qmd`](https://github.com/tbep-tech/habitat-report-card/blob/main/docs/summaries.qmd).  An updaged HTML file called `summaries.html` will be created in the [`docs`](https://github.com/tbep-tech/habitat-report-card/tree/main/docs) folder.  This file can be viewed in a web browser and is the link provided above.
1. Commit and push the updated files to the repository.