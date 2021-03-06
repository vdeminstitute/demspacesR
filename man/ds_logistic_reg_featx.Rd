% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic-reg-featx.R
\name{ds_logistic_reg_featx}
\alias{ds_logistic_reg_featx}
\title{Logistic regression with feature extraction}
\usage{
ds_logistic_reg_featx(space, data, n_comp = 5)
}
\arguments{
\item{space}{(\code{character(1)}) \cr
Name of the V-Dem indicator for a democratic space}

\item{data}{(\code{\link[base:data.frame]{base::data.frame()}})
A data frame or similar object}

\item{n_comp}{the number of components to keep}
}
\description{
Logistic regression with a feature extraction step applied to the training
data, e.g. PCA.
}
\examples{
data("states")

mdl   <- ds_logistic_reg_featx("v2x_veracc_osp", states)
preds <- predict(mdl, new_data = states)
head(preds)

}
\seealso{
Other Other DS models: 
\code{\link{ds_logistic_reg}()},
\code{\link{ds_reg_logreg}()}
}
\concept{Other DS models}
\concept{ds_model}
