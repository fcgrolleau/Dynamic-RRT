# Dynamic-RRT
 
<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-MIT-blue)](https://github.com/fcgrolleau/Dynamic-RRT/blob/main/LICENSE)
[![R badge](https://img.shields.io/badge/Build%20with-%20R,%20♥%20and%20python-blue)](https://rstudio.github.io/reticulate/index.html)
[![R 4.2.1](https://img.shields.io/badge/R-4.2.1-blue.svg)](https://www.r-project.org) 
[![Python 3.8.8](https://img.shields.io/badge/python-3.8.8-blue.svg)](https://www.python.org) 
<!-- badges: end -->

This repository reproduces results from the paper *Personalising renal replacement therapy initiation in the intensive care unit: a statistical reinforcement learning-based dynamic strategy with external validation on the AKIKI randomised controlled trials*.

### Authors
This repository is written and maintained by François Grolleau (francois.grolleau@aphp.fr).

### Reproducibility

- The **Learning** folder includes code related to the section titled “Learning an optimal strategy” in the paper. It contains the following files.

 `dataprep_dev.R` details the preprocessing and multiple imputation of the MIMIC data.

 `data_description_dev.R` reproduces Table 1 and Figure S3 (A) for MIMIC.
 
 `dWOLS.R` details the dWOLS estimation procedure optimal strategy learning in the MIMIC data.

 `figS2_A.R` reproduces Figure S2 (A) for MIMIC (Sankey diagram).

 `figS2_B.R` reproduces Figure S2 (B) for MIMIC (Sankey diagram).

 `export_feather.R` export imputed dataframes as feather files for downstream use in python.

 `functions.R` contains handy functions `defactorize` and `rubinr` to ”“defactorize” a dataframe and compute Rubin's rule respectively. 

- The **Evaluation** folder includes code related to the section titled “External validation” in the paper. It contains the following files.

`algo1.R` implment the EM algorithm from the paper and returns ARE, AIE and MIG estimates 
 ARE, AIE and MIG estimates along their bootstrap standard errors can be obtained in one line of code. 
 An example is given at the end of the file.

`simulations.R` reproduce the simulations given in the paper

`plot_results.R` plots the results of the simulations and reproduce Figure 4 from the paper

### Dynamic decision support system
Available at <a href="http://dynamic-rrt.eu/">http://dynamic-rrt.eu/</a>

The code for this webapp is available in the **Shiny** folder.
### References
Grolleau F, Petit F, Gaudry S, Diard E, Quenot JP, Dreyfuss D, Tran VT, and Porcher R.
<a href="https://medRxiv.org/">*medRxiv*</a>. 2023.

This applied work heavily relies on the following two technical papers:

- Wallace MP and Moodie EEM. Doubly-robust dynamic treatment regimen estimation via weighted least squares. 
*Biometrics* 2015; 71: 636–44.

- Nie X, Brunskill E, and Wager S. Learning when-to-treat policies. *Journal of the American Statistical Association* 2021; 116: 392–409. [<a href="https://arxiv.org/pdf/1905.09751.pdf">*arXiv*</a>].