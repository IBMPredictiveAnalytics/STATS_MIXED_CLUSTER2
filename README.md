<h1>STATS_MIXED_CLUSTER2 Extension Command</h1>

<h2>Introduction</h2>
Use this procedure to create a clustering model where variable selection and number of clusters can be
determined simultaneously.  It can be used to create a latent class model and assign a class for each case in
the estimation data.  The estimated model can be saved and then used to assign classes to new data.
Optionally, the probability of membership in each class can be assigned for each case.  The discriminative power
of each variable is displayed in a table along with an optional plot.  The larger the power, the more
the variable distinguishes the clusters.

<h2>Specifications</h2>

<p>Three types of variables can be used.  Continuous (scale) variables are considered to be drawn
from a normal distribution for likelihood calculation; categorical (nominal and ordinal) variables from a multinomial distribution, and a third type, counts,
are assumed to be Poisson.  The variable type for continuous and scale variables is based
on the declared measurement level in SPSS - scale vs nominal or ordinal.  Counts are
assumed to be Poisson and must have a scale level in SPSS. As Poisson variables, the counts
should be small to moderate in size and nonnegative.  Missing values are managed without any pre-processing
by the model used to cluster with the assumption that values are missing completely at random.

<p>Tip: if you get an error message referring to NaN values being encountered, it is probably
due to having extremely unlikely values in the Poisson variables.  Such variables should be
treated	 as continuous, but also check that any big missing value codes are properly
declared in SPSS.
Use summary statistics and plots by cluster to see what variables have different distributions across the calculated clusters.

<p>Continuous and categorical variables are entered in the Scale and Categorical Variables box,
and count variables are entered in the Count Variables box.

<p>Enter a blank-separated list of numbers of clusters to be considered in the Number of Clusters control.
The procedure will choose the best of the numbers listed and report results for it.

<p>Variable selection is optional. If Yes is specified in the Select Variables control, the
procedure will simultaneously determine the best variables and number of clusters
according to the choice in the Variable Selection Criterion box.  If No is chosen,
all variables will be included.  In variable selection, variables are classified as
either relevant or irrelevant.   A variable is considered relevant if its distribution is 
dependent on the cluster membership variable; otherwise, it is irrelevant.  That means
for continuous variables that the mean  and variance 
 are identical for every cluster and for
categorical and integer variables that the probability distribution (parameters of the multinomial or Poisson distribution) is the same across all clusters.

<p>There are four choices for the Variable Selection Criterion.  AIC and BIC are available whether or not
variable selection is used.  If variable selection is not used, ICL, Integrated Completed Likelihood can
be used; if selection is used, MICL, Maximum Integrated Complete-Data Likelihood, which maximizes
the likelihood over all possible partitions, can be used.

<p>BIC stands for Bayesian Information Criterion, a metric used in clustering to determine the optimal number of clusters and evaluate model quality, particularly in Gaussian Mixture Models (GMMs). It balances goodness-of-fit against model complexity, favoring models with fewer clusters to prevent overfitting.  AIC stands for Akaike Information Criterion (AIC), which is similar to the BIC but has
a smaller complexity penalty.

<p>With MICL, the procedure implements variable selection by maximizing the MICL criterion, which works by alternating between optimizing the partition (classes) and the variable selection.  This ICL-based approach is often more robust than other methods like BIC when clusters overlap, as its inherent penalty for uncertain assignments leads to more stable and interpretable clustering solutions
at the expense of extra execution time.

<p>The optional time limit specifies the maximum time the estimation stage of the procedure
is allowed to run.  If it has not completed by the limit, the procedure will be rudely terminated,
but the SPSS session will continue to be available.  By default, there is no limit.  The elapsed
time for the procedure is reported in the parameters table for procedures that complete.

<p>On the Options tab, specify a name for the cluster variable and optionally a root name in the Cluster Probabilities Root name
box.  If a probabilities root is specified, the probability for each class will be saved under a name of the form
rootname_n, where n is the class number.  The cluster number variable and names of the proability name pattern must
not already exist in the dataset.  Since the number of classes is typically unknown beforehand, names of the form rootname are
prohibited.

<p>Enter a file specification in the Output for Cluster Model control to save the estimated model for
future use in prediction with new data.  Typically, the file name would have extension .rdata, but any
name can be given.

<p>Besides the casewise variables produced by the procedure, a table of discriminative power and
optional corresponding plot can be produced.  The discriminative power is the logarithm of the ratio between the likelihood of a variable being relevant vs. irrelevant. A value of 0 means the likelihoods are the same, indicating no added value in using that variable for clustering. 

<p>
This procedure does not support case weights.   They will be ignored with a warning.  It also does not support split files.
Be aware that this procedure may be time consuming.

<h2>Prediction</h2>
A saved model can be used to assign clusters to new data based on the variables used in estimating
the model by entering a model file specification in the Model File for Prediction box.  All the estimation variables must be included, even those classified as irrelevant, and they must have the same mesurement level and data type as in the estimation data.

<a>When predicting, all the input variable and parameter specifications are taken from
the estimation model, but the cluster and probability names can be different.  An
output model cannot be specified in prediction mode.

<h2>Acknowledgements</h2>
This procedure uses the R VarSelLCM package from CRAN.

<h2>References</h2>
<a href="https://link.springer.com/article/10.3758/s13428-022-01795-7?fromPaywallRec=true">Variable Selection for Model-Based Clustering with Missing Values</a>
<p><a href="https://cran.r-project.org/web/packages/VarSelLCM/VarSelLCM.pdf">Package VarSelLCM</a>

</body>
  <p style="font-size:80%;">
<p>&copy; Copyright Jon K Peck 2026</p>
