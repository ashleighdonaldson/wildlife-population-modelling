# Wildlife Population Modelling in R

This project explores wildlife population dynamics using published demographic data and historical population estimates. The analyses examine population growth, demographic sensitivity, population viability and density dependence, using desert tortoises and Yellowstone grizzly bears as case studies.

All analyses are contained in a single R script, `wildlife_population_modelling.R`.

## Analyses

### Demographic modelling

Published demographic matrices for desert tortoises (*Gopherus agassizii*) are used to project population growth over 30 years. The analysis estimates the population growth rate (λ), stable stage distribution and demographic elasticity, and compares population growth under different fertility scenarios. A sensitivity analysis is also used to examine the influence of changes in final-stage stasis on population growth.

### Stochastic population viability analysis

The desert tortoise demographic matrices are used to simulate 500 population trajectories over 30 years, with fertility conditions varying between years. The analysis calculates the probability of the population declining below a specified threshold and illustrates uncertainty in projected population size.

The starting population, quasi-extinction threshold and probabilities assigned to fertility scenarios are illustrative rather than empirically estimated.

### Population dynamics

Historical Yellowstone grizzly bear (*Ursus arctos*) population estimates are used to investigate population growth and density dependence. Annual population growth rates are calculated, and density-dependent and density-independent models are compared using Akaike's Information Criterion (AIC). The analysis also examines the relationship between population size and growth rate, estimates carrying capacity where appropriate, and evaluates model diagnostics.

## Data

The analyses use published ecological datasets available through the `popbio` R package:

- Desert tortoise demographic projection matrices.
- Historical Yellowstone grizzly bear population estimates.

The original data sources and associated publications should be acknowledged when using these datasets.

## R packages

- `popbio`
- `ggplot2`
- `dplyr`

## Running the analysis

Download `wildlife_population_modelling.R` and open it in RStudio.

Install the required packages:

```r
install.packages(c("popbio", "ggplot2", "dplyr"))
```

Run the script from beginning to end. The script generates population projections, model summaries, diagnostic plots and CSV files containing selected results.

## Limitations

The desert tortoise projections use published demographic parameters but assume an illustrative initial population. The stochastic analysis uses randomly selected fertility scenarios and does not incorporate all sources of environmental and demographic uncertainty.

The grizzly bear analysis provides an exploratory assessment of density dependence. Population estimates may contain observation error, and the models do not explicitly account for all environmental or management-related influences on population growth.
