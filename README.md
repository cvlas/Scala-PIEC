# Scala-PIEC

This project includes an implementation of the Probabilistic Interval-based Event Calculus (PIEC) using the [Scala programming language](http://scala-lang.org), as well as a series of experiments towards the evaluation of the algorithm. This work is part of my M.Sc. project and serves as companion to my M.Sc. thesis.

## General info

Scala-PIEC has been developed on a Debian-based system, using Scala 2.11, Python 2.7 and Unix/Linux BASH shell scripting. For the visualization of the results of the evaluation experiments, [LaTeX](https://www.latex-project.org/), [PGF/TikZ](https://github.com/pgf-tikz/pgf) and [ScalaTIKZ](https://github.com/vagmcs/ScalaTIKZ) have been used.

PIEC originally appears in [Artikis A., Makris E., and Paliouras G., 2019](https://link.springer.com/article/10.1007%2Fs10472-019-09664-4). It has originally been implemented in Python, by Evangelos Makris. File [piec_original_v2.py](piec_original_v2.py) contains the original implementation of the algorithm.

Folder [src/piec](src/piec) contains the new implementation, in Scala, along with several routines for assessing the performance of this interval-based algorithm, by comparing its Activity Recognition results against timepoint-based approaches, like [Prob-EC](https://github.com/JasonFil/Prob-EC) and [MLN-EC](https://users.iit.demokritos.gr/~anskarl/pub/mlnec/). This assessment is performed using data from the [CAVIAR dataset](http://groups.inf.ed.ac.uk/vision/CAVIAR/CAVIARDATA1/).

We apply the two timepoint-based, probabilistic Event Recognition methods -- namely Prob-EC and MLN-EC -- on the CAVIAR dataset and store the recognition results into two separate folders, [Prob-EC data](eval/Prob-EC%20data) and [MLN-EC data](eval/MLN-EC%20data). We then use this data as input to PIEC and observe whether PIEC's interval-based calculations lead to improved Activity recognition or not.
