# Implementation of Sparse Estimation of Many Normal Means (msc research project) 

All code to reproduce results of thesis can be found here. Data from simulations can also be found here. 

## Dependencies

The `itses` package is needed to run code. The `itses` package is a product of the research project. It can be found seperatly in: https://github.com/AmiAhm/itses.

Please refer to the documentation of `itses` for installation instructions. 

### Other pacakge requirements
The following packages are for the `R` code, `latex2exp`, `xtable`. `testthat`, `asus`, `foreach`. Refer to `R-sessionInfo.txt` for more specific information.

In addition image de-noising and network pruning is run in `pyhton` and `jupyter`  with `tensorflow`, `pandas`, `scikit-image`, `matplotlib`, `numpy`, `rpy2` and `pywt`. Notice that thresholding is done by importing helper functions and packages from `R` using `rpy2`.  

## Structure
In the `jupyter` notebook `neural_network_pruning` pruning is implemented  and tested. Further,  results are plotted in `plot_neural_network_pruning_performance`.

In the notebook `image_de-noising`, image de-noising is implemented and tested. 
### R-utility/

Defines helper functions used across other sections. Data-generation can be found here. 

### generate_plots/

Code to generate most stand-alone plots in `R` be found here.

### output/
All output is stored here. That is all .RData objects, all figures, etc.

### jupyter-html-backup/
Html copies of the jupyter notebooks can be found here. 

### simulation_studies/

Implementation of the simulation study on `itses` general performance can be found here.


## About

Made in part of the research project component for the MSc in Statistics of Imperial College London (September 2021).

By: Amir Ahmed, Supervisor: Alastair Young



