#https://www.safaribooksonline.com/library/view/bayesian-analysis-with/9781785883804/ch01s03.html#ch01lvl3sec05

from scipy import stats
import matplotlib.pyplot as plt
import numpy as np

theta_real = 0.35

# Number of trials per experiment
trials = [0, 1, 2, 3, 4, 8, 16, 32, 50, 150]
# Number of heads per experiment
data = [0, 1, 1, 1, 1, 4, 6, 9, 13, 48]



# Choose beta distribution as prior
# It's popular in Bayesian Analysis because it can model
# many different shapes (uniform, Gaussian, U-like, ...)
# and the conjugate prior probability distribution for the
# # Bernoulli, binomial, negative binomial and geometric distributions.
# https://stats.stackexchange.com/questions/47771/what-is-the-intuition-behind-beta-distribution
dist = stats.beta


# Compare different priors.
# Expected value of beta distribution is ( p / (p + q) )
beta_params = [(1, 1), (0.5, 0.5), (20, 20)]


x = np.linspace(0, 1, 100)

fig = plt.figure(figsize=(8, 6))

for idx, N in enumerate(trials):
    if idx == 0:
        plt.subplot(4,3, 2)
    else:
        plt.subplot(4,3, idx+3)
    y = data[idx]
    for (a_prior, b_prior), c in zip(beta_params, ('b', 'r', 'g')):
        p_theta_given_y = dist.pdf(x, a_prior + y, b_prior + N - y)
        plt.plot(x, p_theta_given_y, c)
        plt.fill_between(x, 0, p_theta_given_y, color=c, alpha=0.6)

    plt.axvline(theta_real, ymax=0.3, color='k')
    plt.plot(0, 0, label="{:d} experiments\n{:d} heads".format(N, y), alpha=0)
    plt.xlim(0,1)
    plt.ylim(0,12)
    plt.xlabel(r'$\theta$')
    plt.legend()
    plt.gca().axes.get_yaxis().set_visible(False)
plt.tight_layout()
plt.show()
