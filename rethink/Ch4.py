
# %% [markdown]
# File uses jupyter cells embedded in native code for better version control.
# See https://code.visualstudio.com/docs/python/jupyter-support-py
# File can be converted to inpy (and backwards) via VS Code commands.
# %%
import pandas as pd
from util import download_as_dataframe
d = download_as_dataframe("Howell1")
d.head()

# %% [markdown]
# To get started, model without predictors.
# $\mu$ has no subscript so its the same estimate for all observations.
# Height is like many measures in nature normal distributed because it may be seen as the result
# of a series of small biological steps. Suming those "samples" up from any distribution yields
# a normal distribution ([Central Limit Theorem](https://en.wikipedia.org/wiki/Central_limit_theorem))
#
# $$
# h_i \sim Normal(\mu,\sigma) \\
# \mu \sim Normal(178,20) \\
# \sigma \sim Uniform(0,50)
# $$

# I use my height as prior for mean.
# A $\sigma$ of 20 encodes that 95% of individuals are at least 158cm and at most 189cm.
# Plot the priors:



# %%
from scipy.stats import norm
from seaborn import distplot
x = norm.rvs(loc=178,scale=20,size=1000)
distplot(x)


# %%
