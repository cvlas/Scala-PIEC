<h2>PIEC vs. Prob-EC experiments</h2>

<h3>Enhanced noisy data</h3>

Folder "enhanced_noisy_data" contains the experiments for the modified "enhanced" version of the dataset (i.e., the LLE "abrupt" is manually inserted). These data is used only for the HLE fighting.

Each sub-folder in "./enhanced_noisy_data/" represents a gamma mean value. The dataset contains 16 gamma mean values, ranging from 0.5 to 8.0 with a step of 0.5).
Folder 0.0 represents the output data of Prob-EC without artificially generated noise (outputfile_noise_free) which is used as input in PIEC. 

Inside a gamma mean value folder (0.5-8.0), there are 5 sub-folders. 
  - Folders "enh_all_run1",  "enh_all_run2", ..., "enh_all_run5" are five experiment repetitions on the modified, "enhanced" version of the dataset. These folders contain the output files of Prob-EC (outputfile_smooth, outputfile_intermediate, outputfile_strong) for smooth, intermediate and strong noise, respectively. These data are given to the PIEC algorithm, as input.


<h3>Original noisy data</h3>

Folder "original_noisy_data" contains the experiments for the original version of the dataset (i.e., no modifications to LLE). These data are used for the HLEs: meeting, moving.

Each sub-folder in "./original_noisy_data/“ represents a gamma mean value. The dataset contains 16 gamma mean values, ranging from 0.5 to 8.0 with a step 0.5).
Folder 0.0 represents the output data of Prob-EC without artificially generated noise (outputfile_noise_free) which is used as input in PIEC.

Inside a gamma mean value folder (0.5-8.0), there are 5 sub-folders. 
  - Folders "orig_all_run1",  "orig_all_run2", ..., "orig_all_run5" are five experiment repetitions on the dataset. These folders contain the output files of Prob-EC (outputfile_smooth, outputfile_intermediate, outputfile_strong) for smooth, intermediate and strong noise, respectively. These data are given to the PIEC algorithm, as input.
