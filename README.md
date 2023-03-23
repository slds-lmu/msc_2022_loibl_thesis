# Interpretation of black box models using tree-based surrogate models
This repository contains code, data and figures for my Masters's Thesis "Interpretation of black box models using tree-based surrogate models"

    ├── R/                                   # All implemented methods and general helper functions                          
    |   ├── tree_splitting_slim.R            # Function to generate slim tree            
    |   ├── load_packages.R                  # load all necessary packages      
    |   ├── helper_general.R                 # helper functions for tree splitting and for the evaluation of slim trees    
    |   ├── helper_guide.R                   # guide specific helper functions for tree splitting    
    |   ├── simulations/                         
    |   |   ├── chapter_4_selection_bias/    # Files for generation and analysis of batchtools experiments for selection bias (chapter 4 and Appendix)
    |   |   ├── chapter_5_simulation_study/  # Files for generation and analysis  of batchtools experiments for simulation study (chapter 5 and Appendix)   
    |   ├── real_world_examples
    ├── Data/simulations/                                    
    │   |   ├── chapter_4_selection_bias/    # Location where generated data of batchtools experiments for selection bias are stored    
    │   |   ├── chapter_5_simulation_study/  # Location where generated data of batchtools experiments for simulation study are stored
    ├── Figures/
    │   |   ├── chapter_4_selection_bias/          
    │   |   ├── chapter_5_simulation_study/  # Location where generated data of 1. simulation example (VINE vs. REPID) are stored
    │   ├── batchtools/                      # Location where generated data of batchtools experiments are stored
    └── README.md 
    
To reproduce the simulation studies run the files "batchtools_....R" in the simulations folder.
Afterwards run the file "analysis_....R" in the simulations folder, to aggregate the data of one experiment and create the figures shown in the thesis.


To fit a single slim tree, source the file "tree_splitting_slim.R" and run the function compute_tree_slim(...). A description of the input parameters can be found in the file "tree_splitting_slim.R"
