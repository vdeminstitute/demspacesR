# demspacesR 0.4

Development during the spring 2022 update.

- Added positive rates to `score_ds_fcast()`. Helpful for interpreting the AUC-ROC and AUC-PR values. 
- Allow sidestepping of the "mtry" tuning in `ds_rf()`. In the last version, "mtry" was auto-tuned over a grid of 13 potential values. I added a "mtry" argument to `ds_rf()` that allows sidestepping this tuning process with a hard coded value instead. This is to make ad-hoc modeling and tuning experiments quicker and easier.
- Add optional "cutpoint" argument to `predict.ds_rf()` that allows setting impossible forecasts to 0. For example, Tunisia in an earlier version of the forecasts had a high risk for Governing opening event, even though it's Governing space score was so high already that given the cutpoint used for that space, no opening even was actually possible. (https://github.com/vdeminstitute/demspaces/issues/15)

# demspacesR 0.3

Development in February 2021. 

- Moved package to **vdeminstitute** GitHub account; package renamed from **demspaces** to **demspacesR**
- Added mean log loss to `score_ds_fcast()`. This is a better metric for comparing models to each other than AUC-ROC/PR. 

# demspacesR 0.2 

Development in November 2019.

* Added a tuner for `ds_rf()` that picks optimal `mtry` values. 

# demspacesR 0.1

Development in October 2019. 

* `reg_logreg()` and `ds_reg_logreg()` implement a self-tuning regularized logistic regression model based on the **glmnet** package. Tuning is performed via cross-validation and successively picks alpha and then lambda values. 
* `rf()` and `ds_rf()` implement a random forest probability tree model using the **ranger** package. 

# demspacesR 0.0.1

Initial version in September 2019. This had the following models:

* `logistic_reg()` is a standard logistic regression model, with `ds_logistic_reg()` as a wrapper for modeling democratic spaces. It includes an option to standardize features prior to model estimation ("normalize" argument), using a standardizer function made by `make_standardizer()`. 
* `logistic_reg_featx()` and `ds_logistic_reg_featx()` are standard logistic regression models with a feature extraction pre-processing step for the input feature data. This uses PCA (the only method implemented currently) to reduce the number of numeric input features to 5, via `make_extract_features()`. 

The GitHub repo, but not package, includes a template for adding new models in `add_new_model.R`. 
