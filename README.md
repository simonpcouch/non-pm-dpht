
## Power Analyses of Differentially Private Nonparametric Hypothesis Tests

This repository contains source code to carry out the power analyses of
several differentially private hypothesis testing algorithms from the
2019 paper *Differentially Private Nonparametric Hypothesis Testing*
\[1\].

The following conventions have been maintained throughout the repository
in order to help the reader follow along.

  - Source code is divided up by hypothesis test.
      - `anova` refers to our implementation of Swanberg et. al.’s
        differentially private ANOVA test \[2\], developed for the 3 or
        more group parametric setting.
      - `kw` refers to our tests developed for the 3 or more group
        nonparametric setting, analogous to the public Kruskal-Wallis
        test.
      - `mw` refers to our tests developed for the 2 group, unpaired
        nonparametric setting, analogous to the public Mann-Whitney
        test.
      - `t` refers to our tests developed for the 2 group, paired
        parametric setting, analogous to the public paired t-test.
      - `wc` refers to our tests developed for the 2 group, paired
        nonparametric setting, analogous to the public Wilcoxon
        Signed-Rank test (as well as our implementation of Task and
        Clifton’s test \[3\].)
  - The test subfolders generally follow the same organization.
      - In each, there is a `data` subsubfolder containing datasets
        resulting from the simulations and analyses.
      - In some, too, there is a `fxns` subsubfolder–this folder
        contains functions that are sourced throughout the repository.
        In particular, two appear many times. `gen_data` simulates many
        trials worth of sample data with given parameters, and
        `calc_stat` takes in a dataframe outputted by `gen_data` and
        outputs a distribution of test statistics. These two functions
        can be mapped over various parameterizations (this wrapping is
        usually referred to as `pwr_plot`) to generate a reference
        distribution, calculate critical values, generate an alternate
        distribution, and calculate power. Note that several
        `gen_data`s, `calc_stat`s, and `pwr_plot`s exist for different
        tests throughout this repository—the hope is that this
        consistency invokes the fact that these functions serve the same
        purpose in different settings, even though they are named
        redundantly.
      - With a few exceptions, `.Rmd` files contain implementations of
        the full `gen_data`-`calc_stat` framework in order to conduct
        power analyses of our tests. These files generally contain the
        code to be used for a subset of the plots needed, and one can
        pick up on naming conventions after spending a short time with
        the file structure.
  - All figures (exclusively saved to the `figures` folder) and
    dataframes are saved with the prefix(es) denoting the relevant
    tests. Dataframe objects are always stored under the same name as
    the file they are saved to. When there is an explicit pairing
    between a plot and a dataframe, they are given the same name.
  - On several occasions, we make comparisons between tests. In these
    cases, when possible, we consolidate code for plots in the folder of
    whichever test came first in the order presented in the paper
    (Kruskal Wallis, ANOVA, Mann-Whitney, Wilcoxon, Paired-t.) However,
    note that all comparisons to ANOVA happen in the `anova` folder.
  - All files paths are written such that the document can be executed
    with the parent folder as the working directory.

Note that these files are slightly modified versions of the original
ones used to conduct the power analyses in order to make this repository
more easily navigable to readers. Comments have been added, files have
been given more informative names, and file structure has been
simplified. I’ve done my best to ensure that file sourcing and saving
still operates as intended, but if I missed something, feel free to open
an issue and let me know.

Please note, too, that calls to `pwr_plot` *should* take a significant
amount of time to run. To reduce running time (being aware that this
will result in less precise plotting points), the `reps` argument can be
decreased.

\[1\] Simon Couch, Zeki Kazan, Kaiyan Shi, Andrew Bray, and Adam Groce.
“Differentially Private Nonparametric Hypothesis Testing.” arXiv.
2019.

\[2\] Marika Swanberg, Ira Globus-Harris, Iris Griffith, Anna Ritz, Adam
Groce, and Andrew Bray. “Improved Differentially Private Analysis of
Variance.” In *Proceedings on Privacy Enhancing Technologies Symposium.*
2019.

\[3\] Christine Task and Chris Clifton. “Differentially Private
Significance Testing on Paired-Sample Data.” In *Proceedings of the
Society for Industrial and Applied Mathematics’ International Conference
on Data Mining.* 2016.
