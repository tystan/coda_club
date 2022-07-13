
# ---- libs ----


library("dplyr") # tibble/data.frames
library("knitr") # pretty printing of data.frames using kable(.)

# ---- define_terms ----

stats_terms_dictionary <-
  tribble(
    ~term, ~description,
    "univariate", "uni=one --> single variable",
    "descriptive analysis/statistics", "generally centrality (mean, median etc) and spread (sd, IQR) stats",
    "(multi)collinearity", "predictor variables that are highly associated",
    "logratios", "ratio variables that have had the log transform applied",
    "sample space", "a conceptual 'space' of possible values you might observe",
    "real space", "normally refers to cartesian/Euclidean coordinates (e.g. x-y plane which is 2D real space",
    "Simplex", "A bounded triangular space (or generalisations for higher dimensions) ",
    "Gaussian distribution", "a fancy way to say the normal distribution",
    "perturbation", "a maths operation in relative space analogous to +/- in absolute space",
    "powering", "a maths operation in relative space analogous to multiplication/division in absolute space",
    "arithmetic mean", "the technical term for the normal mean we use = sum(values) / n",
    "geometric mean", "an alternative mean that makes more sense in a relative sense = nth_root(product(values))",
    "Euclidean distance", "distance between points in real/absolute space (e.g. Euclid distance between 2 and 5 is 3)",
    "MANOVA", "multivariate analysis of variance (generalisation of the univariate ANOVA)",
    "iso-contours", "iso=equal so equally distanced (in density) contours of a location",
    "Aitchison distance", "distance on a relative scale (not absolute)",
    "(sample) variation matrix", "a matrix to explain the pairwise variation of compositional parts in a sample",
    "ilrs: isometric log ratios", "isometric (meaning 'equally measured') log ratios: a specific transformation of compositional parts",
    "pivot", "in CoDA: analysis with one compositional part (e.g. sleep) as the central focus",
    "multivariate linear regression", "a statistical model",
    "coefficient", "a constant value that is multiplied against a variable"
  )

# ---- print_terms ----

kable(stats_terms_dictionary[order(stats_terms_dictionary$term),])

### output should look like this (best viewed in text editor at least 140 characters wide):

# |term                            |description                                                                                         |
# |:-------------------------------|:---------------------------------------------------------------------------------------------------|
# |(multi)collinearity             |predictor variables that are highly associated                                                      |
# |(sample) variation matrix       |a matrix to explain the pairwise variation of compositional parts in a sample                       |
# |Aitchison distance              |distance on a relative scale (not absolute)                                                         |
# |arithmetic mean                 |the technical term for the normal mean we use = sum(values) / n                                     |
# |coefficient                     |a constant value that is multiplied against a variable                                              |
# |descriptive analysis/statistics |generally centrality (mean, median etc) and spread (sd, IQR) stats                                  |
# |Euclidean distance              |distance between points in real/absolute space (e.g. Euclid distance between 2 and 5 is 3)          |
# |Gaussian distribution           |a fancy way to say the normal distribution                                                          |
# |geometric mean                  |an alternative mean that makes more sense in a relative sense = nth_root(product(values))           |
# |ilrs: isometric log ratios      |isometric (meaning 'equally measured') log ratios: a specific transformation of compositional parts |
# |iso-contours                    |iso=equal so equally distanced (in density) contours of a location                                  |
# |logratios                       |ratio variables that have had the log transform applied                                             |
# |MANOVA                          |multivariate analysis of variance (generalisation of the univariate ANOVA)                          |
# |multivariate linear regression  |a statistical model                                                                                 |
# |perturbation                    |a maths operation in relative space analogous to +/- in absolute space                              |
# |pivot                           |in CoDA: analysis with one compositional part (e.g. sleep) as the central focus                     |
# |powering                        |a maths operation in relative space analogous to multiplication/division in absolute space          |
# |real space                      |normally refers to cartesian/Euclidean coordinates (e.g. x-y plane which is 2D real space           |
# |sample space                    |a conceptual 'space' of possible values you might observe                                           |
# |Simplex                         |A bounded triangular space (or generalisations for higher dimensions)                               |
# |univariate                      |uni=one --> single variable                                                                         |
