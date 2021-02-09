# demspaces 0.2 

Development in November 2019.

* Added a tuner for `ds_rf()` that picks optimal `mtry` values. 

# demspaces 0.1

Development in October 2019. 

* `reg_logreg()` and `ds_reg_logreg()` implement a self-tuning regularized logistic regression model based on the **glmnet** package. Tuning is performed via cross-validation and successively picks alpha and then lambda values. 
* `rf()` and `ds_rf()` implement a random forest probability tree model using the **ranger** package. 

# demspaces 0.0.1

Initial version in September 2019. This had the following models:

* `logistic_reg()` is a standard logistic regression model, with `ds_logistic_reg()` as a wrapper for modeling democratic spaces. It includes an option to standardize features prior to model estimation ("normalize" argument), using a standardizer function made by `make_standardizer()`. 
* `logistic_reg_featx()` and `ds_logistic_reg_featx()` are standard logistic regression models with a feature extraction pre-processing step for the input feature data. This uses PCA (the only method implemented currently) to reduce the number of numeric input features to 5, via `make_extract_features()`. 

The GitHub repo, but not package, includes a template for adding new models in `add_new_model.R`. 
