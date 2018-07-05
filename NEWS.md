# JointAI 0.2.0
## Bug fixes
* `md_pattern()`: does not generate duplicate plot any more
* corrected names of imputation methods in help file
* scaling when no continuous covariates are in the model or scaling is deselected fixed
* initial value specification for coefficient for auxiliary variables fixed
* `get_MIdat()`: imputed values are now filled in in the correct order
* `get_MIdat()`: variables imputed with `lognorm` are now included when extracting an imputed dataset
* `get_MIdat()`: imputed values of transformed variables are now included in imputed datasets
* problem with non valid names of factor labels fixed
* data matrix is now ordered according to order in user-specified `meth` argument

## Minor changes
* `md.pattern()`: adaptation to new version of `md.pattern()` from the **mice** package
* internally change all `NaN` to `NA`
* allow for scaling of incomplete covariates with quadratic effects
* changed hyperparameter for precision in models with logit link from 4/9 to 0.001

## Extensions
* `gamma` and `beta` imputation methods implemented