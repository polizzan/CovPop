# Projecting the Long-term Effects of the COVID-19 Pandemic on U.S. Population Structure

## Purpose of this Repository
The repository **CovPop** was created to enable readers to replicate the findings reported in:

*Projecting the Long-term Effects of the COVID-19 Pandemic on U.S. Population Structure*,

hereafter, *our manuscript*.

## Repository Structure
The repository **CovPop** contains two main folders, *Journal* and *SocArXiv*.

### *1. Journal*
The main folder **Journal** contains all data and `R` scripts necessary to replicate the information reported in the journal version of our manuscript. This includes the information reported in the Supplementary Information and the point-by-point responses to the peer reviewers&apos; comments. The data and analysis files are respectively stored in the sub-folders *data* and *scripts*.

For our main analysis, we use data from the [United Nations World Population Prospects](https://population.un.org/wpp/) (UNWPP), version 2022, provided by the United Nations Department of Economic and Social Affairs (Ref. 1).

For our supplementary analysis, we also use data from the [Human Mortality Database](https://mortality.org), version 03 April 2023 (Ref. 2), and the [Human Fertility Database](https://humanfertility.org), deposited on 28 March 2023 (Ref. 3), as well as data published in three National Vital Statistics Reports (Ref. 4&ndash;6).

The UNWPP data used for our analysis are distributed under a [Creative Commons license CC BY 3.0 IGO](https://creativecommons.org/licenses/by/3.0/igo/). The HMD and HFD data used for our analysis are distrubuted under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). Please note the version or deposited dates of the data used for our analysis, as indicated above.

#### a. *data*
Data taken from UNWPP, HMD, and HFD are stored as .zip files in the sub-folder **data**. These .zip files  do **not** need to be unzipped before running our analysis files, as the code unzips these files automatically. Data from the three National Vital Statistics Reports are hard coded in our analysis files. The sub-folder **data** also stores data files created during  analysis.

#### b. scripts
The sub-folder **scripts** contains the `R` scripts necessary to replicate all findings reported in our main manuscript, Supplementary Information, and/or the point-by-point responses to the peer reviewers&apos; comments:

- **99-functions.R**
Creates custom functions used to calculate survivorship ratios, conduct stochastic or deterministic population projections, and build some of our plots.

- **01-preparation.R**
Loads UNWPP data, prepares them for further analysis, and saves them in a combined data file *projection-input.RData*.

- **02-projections.R**
Carries out all counterfactual population projections, derives summary indicators from the counterfactual population projections, and saves these summary indicators in a combined data file *projection-output.RData*.

- **03-out.R**, **98-compare-sources-rr1.R**, **98-compare-sources-rr2.R**
These three files create all plots and tables reported in our main manuscript, Supplementary Information, and/or or the point-by-point responses to the peer reviewers&apos; comments. All output is stored in the automatically generated folder **out**.

- **00-main.R**
Installs all `R` packages necessary to replicate our findings, loads user input (such as the selected color palettes and random seeds), and automatically executes all analysis files listed above.

- **03-out-pub.R**, **99-functions-pub.R**
These two stand-alone files create all plots reported in the journal version of our article, following the journal&apos;s formatting requirements. The file `03-out-pub.R` installs all `R` packages necessary for plotting and automatically executes the file `99-functions-pub.R`.

### 2. *SocArXiv*
The main folder **SocArXiv** contains all materials associated with version 1 of our manuscript preprint, as posted on *SocArXiv* (Ref. 7). This main folder is just for reference, as the analytical strategy and code have changed following the peer review of our manuscript.

## How to Use this Repository
In order to run the `R` code provided in the repository **CovPop**, please proceed in the following order:

1. Download the repository from `github`. If applicable, unzip the downloaded folder and place it in a location convenient for you. 
2. Double click on the file `CovPop.Rproj` in the **Journal** folder. This should open `RStudio` on your machine.  
3. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `00-main.R` located in the **scripts** sub-folder.
4. You should now be able to run our code without adjusting any directories.

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png

## References
1. United Nations, Department of Economic and Social Affairs, Population Division, World Population Prospects. https://population.un.org/wpp/. Version 2022.
2. Max Planck Institute for Demographic Research (Germany), University of California, Berkeley (USA), French Institute for Demographic Studies (France), Human Mortality Database (HMD). https://mortality.org. Version 03 April 2023.
3. Max Planck Institute for Demographic Research (Germany) and Vienna Institute of Demography (Austria), Human Fertility Database (HFD). https://humanfertility.org. Deposited 28 March 2023.
4. M.J.K. Osterman, B.E. Hamilton, J.A. Martin, A.K. Driscoll, C.P. Valenzuela, Births: Final Data for 2021. *National Vital Statistics Reports* **72**(1) (2023). 
5. E. Arias, J. Xu, United States Life Tables, 2019. *National Vital Statistics Reports* **70**(19) (2022).
6. E. Arias, J. Xu, United States Life Tables, 2020. *National Vital Statistics Reports* **71**(1) (2022).
7. A.M. Tilstra, A. Polizzi, S. Wagner, E.T. Akimova, The Long-term Effects of the COVID-19 Pandemic on U.S. Population Structure. *SocArXiv*. https://doi.org/10.31235/osf.io/rqn9j. Version 1. Submitted 28 March 2023.