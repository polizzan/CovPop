# The Long-term Effects of the COVID-19 Pandemic on U.S. Population Structure

## Purpose of the Repository

The repository **CovPop** was created to provide supplementary figures for our manuscript:

*The Long-term Effects of the COVID-19 Pandemic on U.S. Population Structure*

and enable readers to replicate our findings.

## Repository Structure
The repository **CovPop** contains two main folders:

- **Supplementary Figures**

- **Replication**

## Main Folder 'Supplementary Figures'
The main folder **Supplementary Figures** contains supplementary figures that are not reported in our manuscript. These are stored in two sub-folders:

- **absolute**

	contains four .gif files displaying the results shown in Figures 2a and 3a in moving image form: "Figure-2-absolute.gif"; "Figure-3-Mortality-absolute.gif"; "Figure-3-Fertility-absolute.gif"; "Figure-3-Migration-absolute.gif"	

- **relative**

	contains four .gif files displaying the results shown in Figures 2b and 3b in moving image form: "Figure-2-relative.gif"; "Figure-3-Mortality-relative.gif"; "Figure-3-Fertility-relative.gif"; "Figure-3-Migration-relative.gif".

## Main Folder 'Replication'
The main folder **Replication** contains the data and `R` scripts necessary to replicate our findings. These are stored in two sub-folders:

- **data**

	contains all data files necessary to replicate our findings

- **scripts** 

	contains all `R` scripts necessary to replicate our findings

The main folder **Replication** also provides empty sub-folders needed to seamlessly execute our `R` scripts, e.g., to save plots:

- **plots**

	stores all plots created by our `R` scripts (see sub-folder **scripts**) and shown in our manuscript

### Sub-folder 'data'
We use data provided by the 2022 **United Nations World Population Prospects** (UNWPP). The raw data used for our analysis can be downloaded manually from: https://population.un.org/wpp/.

### Sub-folder 'scripts'
The sub-folder **scripts** contains the `R` scripts necessary to replicate all findings reported in our manuscript, as well as the supplementary figures described above:

- **00-main.R**

	installs all `R` packages necessary to replicate our findings. This `R` script automatically executes the files `01-preparation.R`, `02-projections.R`, and `99-functions.R`, which are described in more detail below.

- **01-preparation.R**

	loads the necessary data provided by UNWPP, prepares them for further analysis, and saves them in a combined data file "Proj.RData". This `R` script assumes that all data were manually downloaded and stored as .csv or .zip files in the sub-folder **data** following our naming conventions.

- **02-projections.R**

	replicates all findings reported in the section **Results** of our manuscript and creates and saves all (supplementary) figures

- **99-functions.R**

	creates the functions used to calculate survivorship ratios, conduct the projections, and create some of our plots. This `R` script must therefore be executed before running the `R` scripts `01-preparation.R` and `02-projections.R`.

## How to Use the Repository
In order to run the `R` code provided in the repository **CovPop**, please proceed in the following order:

1. Open the file `CovPop.Rproj` in `RStudio`.  
2. Within `RStudio`, click on `File`/`Open File...` and select the file `00-main.R` located in the **scripts** folder.
3. Run the code.