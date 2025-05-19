# rmlnomogram: An R package to construct an explainable nomogram for any machine learning algorithms

![CRAN Version](https://www.r-pkg.org/badges/version/rmlnomogram)
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rmlnomogram)
![Monthly Downloads](https://cranlogs.r-pkg.org/badges/last-month/rmlnomogram)

`rmlnomogram` is an R package designed to construct an explainable nomogram for a machine learning (ML) model to improve availability of an ML prediction model in addition to a computer application, particularly in a situation where a computer, a mobile phone, an internet connection, or the application accessibility are unreliable. This package enables a nomogram creation for any ML prediction models, which is conventionally limited to only a linear/logistic regression model. This nomogram may indicate the explainability value per feature, e.g., the Shapley additive explanation value, for each individual. However, this package only allows a nomogram creation for a model using categorical without or with single numerical predictors (https://doi.org/10.48550/arXiv.2501.05772).


## Features

- **Construct nomogram from machine learning algorithms**: Construct a nomogram for either binary or continuous outcomes based on provided sample features/predictors and outputs.

- **Incorporate model explainability**: We can also incorporate feature/predictor explainability values, such as SHAP values, for a nomogram with probability.


## Installation

You can install `rmlnomogram` from CRAN with:

```r
install.packages("rmlnomogram")
```

You can install the development version of `rmlnomogram` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("herdiantrisufriyana/rmlnomogram")
```


## Quick Start

Load necessary packages.

```r
library(rmlnomogram)
```

Nomogram for categorical predictors and binary outcome without probability.

```r
data(nomogram_features)
data(nomogram_outputs)
create_nomogram(nomogram_features, nomogram_outputs)
```

Nomogram for categorical predictors and binary outcome with probability.

```r
create_nomogram(nomogram_features, nomogram_outputs, prob = TRUE)
```

Add model explainability.

```r
data(nomogram_shaps)
create_nomogram(nomogram_features, nomogram_outputs, nomogram_shaps, prob = TRUE)
```

Nomogram for categorical and 1 numerical predictors and binary outcome with probability.

```r
data(nomogram_features2)
data(nomogram_outputs2)
create_nomogram(nomogram_features2, nomogram_outputs2, prob = TRUE)
```

Add model explainability.

```r
data(nomogram_shaps2)
create_nomogram(nomogram_features2, nomogram_outputs2, nomogram_shaps2, prob = TRUE)
```

Nomogram for categorical predictors and continuous outcome.

```r
data(nomogram_features3)
data(nomogram_outputs3)
create_nomogram(nomogram_features3, nomogram_outputs3, est = TRUE)
```

Add model explainability.

```r
data(nomogram_shaps3)
create_nomogram(nomogram_features3, nomogram_outputs3, nomogram_shaps3, est = TRUE)
```

Nomogram for categorical and 1 numerical predictors and continuous outcome

```r
data(nomogram_features4)
data(nomogram_outputs4)
create_nomogram(nomogram_features4, nomogram_outputs4, est = TRUE)
```

Add model explainability.

```r
data(nomogram_shaps4)
create_nomogram(nomogram_features4, nomogram_outputs4, nomogram_shaps4, est = TRUE)
```


## Vignettes

Explore detailed examples and methodologies in the following vignettes:

- [**Machine Learning Nomogram Exemplar**](https://htmlpreview.github.io/?https://github.com/herdiantrisufriyana/rmlnomogram/blob/master/doc/ml_nomogram_exemplar.html): A guide through basic scenarios demonstrating how to use `rmlnomogram` to create a nomogram.

- [**Reference Manual**](https://github.com/herdiantrisufriyana/rmlnomogram/blob/master/extras/rmlnomogram_0.1.3.pdf): Comprehensive documentation of all functions and features available in `rmlnomogram`. Ideal for detailed reference and advanced use cases.


## License

`rmlnomogram` is licensed under the MIT license. See the LICENSE file for more details.


# Citation

If you use `rmlnomogram` in your research, please consider citing it:

```bibtex
@article{rmlnomogram2025,
  author = {Herdiantri Sufriyana and Emily Chia-Yu Su},
  title = {rmlnomogram: An R package to construct an explainable nomogram for any machine learning algorithms},
  year = {2025},
  journal = {arXiv preprint},
  volume = {arXiv:2501.05772},
  doi = {10.48550/arXiv.2501.05772},
  url = {https://doi.org/10.48550/arXiv.2501.05772}
}
```

Preprint of an article published in the 20th World Congress on Medical and Health Informatics: MedInfo 2025.


## Contact

For questions or support, please contact herdi[at]nycu.edu.tw.


# Programming Environment

## System requirements

Install Docker desktop once in your machine. Start the service every time you build this project image or run the container.

## Installation guide

Build the project image once for a new machine (currently support AMD64 and ARM64).

```{bash}
docker build -t rmlnomogram --load .
```

Run the container every time you start working on the project. Change left-side port numbers for either Rstudio or Jupyter lab if any of them is already used by other applications.

In terminal:

```{bash}
docker run -d -p 8787:8787 -p 8888:8888 -v "$(pwd)":/home/rstudio/project --name rmlnomogram_container rmlnomogram
```

In command prompt:

```{bash}
docker run -d -p 8787:8787 -p 8888:8888 -v "%cd%":/home/rstudio/project --name rmlnomogram_container rmlnomogram
```

## Instructions for use

### Rstudio

Change port number in the link, accordingly, if it is already used by other applications.

Visit http://localhost:8787.
Username: rstudio
Password: 1234

Your working directory is ~/project.

### Jupyter lab

Use terminal/command prompt to run the container terminal.

```{bash}
docker exec -it rmlnomogram_container bash
```

In the container terminal, run jupyter lab using this line of codes.

```{bash}
jupyter-lab --ip=0.0.0.0 --no-browser --allow-root
```

Click a link in the results to open jupyter lab in a browser. Change port number in the link, accordingly, if it is already used by other applications.






