# Dynamic-RRT
 
<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-MIT-blue)](https://github.com/fcgrolleau/Dynamic-RRT/blob/main/LICENSE)
[![R badge](https://img.shields.io/badge/Build%20with-%20R,%20♥%20and%20Python-blue)](https://rstudio.github.io/reticulate/index.html)
[![R 4.2.1](https://img.shields.io/badge/R-4.2.1-blue.svg)](https://www.r-project.org) 
[![Python 3.8.8](https://img.shields.io/badge/Python-3.8.8-blue.svg)](https://www.Python.org) 
<!-- badges: end -->

This repository reproduces results from the paper *Personalising renal replacement therapy initiation in the intensive care unit: a statistical reinforcement learning-based dynamic strategy with external validation on the AKIKI randomised controlled trials*.

### Authors
This repository is written and maintained by François Grolleau (francois.grolleau@aphp.fr).

### Reproducibility

- The **Learning** folder includes code related to the section titled “Learning an optimal strategy” in the paper.

It contains the following files.

 `dataprep_dev.R` details the preprocessing and multiple imputations of the MIMIC data.

 `data_description_dev.R` reproduces Table 1 and Figure S3 (A) for the MIMIC.
 
 `dWOLS.R` details the dWOLS estimation procedure for optimal strategy learning in the MIMIC data.

 `figS2_A.R` reproduces Figure S2 (A) for MIMIC.

 `figS2_B.R` reproduces Figure S2 (B) for MIMIC.

 `export_feather.R` export the imputed development dataframes as feather files for downstream use in Python.

 `functions.R` contains handy functions `defactorize` and `rubinr` to “defactorize” dataframes and compute Rubin's rule respectively. 
<br><br>

- The **Evaluation** folder includes code related to the section titled “External validation” in the paper.

It contains the following files.

 `akiki_1_2_preprocessing.R` details the preprocessing and multiple imputations of the AKIKI trial data.

 `data_description_val.R` reproduces Table 1 and Figure S3 (B) for the AKIKI trial data.
 
 `get_predictions_val.R` get recommendations from the “crude” and “stringent” learned strategies, and export the imputed validation dataframes as feather files for downstream use in Python.

 `n_rrt_eval.py` details importance sampling for policy evaluation for the proportion of patients who would initiate renal replacement therapy within three days, using the AKIKI trial data.

 `hmor_eval.py` details importance sampling for policy evaluation for hospital mortality, using the AKIKI trial data. This file is adapted from `n_rrt_eval.py`.

 `crossfit_ADR_eval.py` details the cross-fitted advantage doubly robust estimator for strategy evaluation with terminal states for hospital-free days at day 60, using the AKIKI trial data.

 `fig2.R` reproduces Figure 2 of the paper.

 `fig3_A.R` reproduces Figure 3 (A) of the paper.

 `fig3_B.R` reproduces Figure 3 (A) of the paper.

 `fig4.R` reproduces Figure 2 of the paper.
<br><br>

- The **Shiny** folder includes the code producing the webapp for our dynamic decision support system.

It contains the following files and folders.

 `ui.R` for the user interface shiny file.

 `server.R` for the server shiny file.
 
 `www` a folder that contains the `cress.png` and `uparis.png` logos.

 `img` a folder that contains screenshots from our webapp.

### Sankey diagrams

Sankey diagrams were produced with the files corresponding to Figure 3 and S2 mentioned above. Then, HTML files were imported as resolution-independent images with <a href="https://nytimes.github.io/svg-crowbar/">SVG Crowbar</a> and colored in Adobe Illustrator.

### Bootstrapping

Bootstrapping was conducted with the `Speedboot` Python library that we developed during the course of this study. The library is available on <a href="https://pypi.org/project/speedboot/">Pypi</a> with documentation accessible <a href="https://github.com/fcgrolleau/speedboot">here</a>.

### Decision support system
<a href="http://dynamic-rrt.eu"><img src="https://fcgrolleau.github.io/Dynamic-RRT/Shiny/img/img.jpg"/></a>

Available at <a href="http://dynamic-rrt.eu">http://dynamic-rrt.eu/</a>.

### References
Grolleau F, Petit F, Gaudry S, Diard E, Quenot JP, Dreyfuss D, Tran VT, and Porcher R.
<a href="https://medRxiv.org/">*medRxiv*</a>. 2023.

This applied work heavily relies on the following two technical papers:

- Wallace MP and Moodie EEM. Doubly-robust dynamic treatment regimen estimation via weighted least squares. 
*Biometrics* 2015; 71: 636–44.

- Nie X, Brunskill E, and Wager S. Learning when-to-treat policies. *Journal of the American Statistical Association* 2021; 116: 392–409. [<a href="https://arxiv.org/pdf/1905.09751.pdf">*arXiv*</a>].