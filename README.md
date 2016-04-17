# hacktoolkit

Potentially useful stuff for the next Allstate Hackathon

## Installation

```R
# install.packages("devtools")
devtools::install_github("DexGroves/hacktoolkit")
```

## What's in here

Weighted variance, weighted binomial deviance. `weighted_variance`, `binomial_deviance`

A pretty fast implementation of weighted AUC. `auc`

Logistic conveniences. `logit_to_prob`, `prob_to_logit`

Pretty efficient leave-one-out encoding. `loo_encode`

Somewhat-efficient-but-horrifically-slow-by-nature marginal effect algorithm with sampling. `marginal_effect`

Tools for training models and scoring out-of-fold for use in downstream models/metamodels. `cross_train_predict`, `train_folds`, `predict_folds`
