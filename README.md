# SPA_STOCSY

SPA_STOCSY is a R pipeline for the automatic identification of both "known" and "unknown" metabolites in given samples.  
With given input samples, SPA_STOCSY offers automatic algorithms to estimate all the parameters to generatet the final results.  
Here is an example of using SPA_STOCSY to analyze on 10 NMR samples from brain homogenates of Drosophila melanogaster.  
[Click here](https://wanliw96.github.io/SPA_STOCSY/spa_stocsy_menu.html)

## Data

All the data listed in the paper are stored in input_data/ foler. 
- 10_meta_simulation.csv : 50 synthesized samples from 10 metabolites
- htt_c12.csv: 10 NMR samples from brain homogenates of Drosophila melanogaster
- human_cell_data.csv: 22 NMR samples from human cells  
- chx_lib_trunc.csv: Chenomx library fror 257 metabolites with their cluster information  

## Input format

- The input file takes multiple NMR samples as columns. With the first column specified as a list of chemical shift values.
- The NMR samples are highly recommended to get normalized on integral.






