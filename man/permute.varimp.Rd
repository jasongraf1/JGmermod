\name{permute.varimp}
\alias{permute.varimp}
\title{Permutation variable importance for regression}
\usage{
permute.varimp(fit, data = NULL, verbose = FALSE, ranef = TRUE)
}

\description{
Calculate variable importance in a model by randomly permuting the values of each variable.  
}

\arguments{
\item{fit}{ a object of class \code{lmerMod}, \code{glmerMod}, \code{glm}, \code{lm}, or \code{lrm}. }
\item{data}{ the dataframe on which \code{fit} was estimated. Only necessary for \code{lrm} model objects. }
\item{verbose}{ logical. If \code{TRUE} the function will print updates when each predictor has been estimated.}
\item{ranef}{ logical. If \code{fit} is a \code{merMod} object, should the random effects terms be included in the ranking?}
}

\details{
For each predictor in the model, the values of that predictor are randomly permuted to break their association with the response, and the model is re-fit to a new dataset containing the permuted values. The fit of the new model is compared to that of the original model. Three measures of model fit are calculated: The concordance C, Accuracy (% correctly predicted), and AICc.} 
}

\value{
A dataframe with 3 columns and \emph{k} rows, where \emph{k} is the number of individual predictors in the model. Columns are as follows:
\item{Predictor}{ name of predictor }
\item{C}{ difference in the concordance statistic C (a.k.a. AUC) between the original model and the model with the permuted predictor}
\item{accuracy}{ difference in the proportion of observations correctly predicted between the original model and the model with the permuted predictor}
\item{AICc}{ difference in the conditional AIC (see Burnham and Anderson 2002) between the original model and the model with the permuted predictor}
}

\author{J. Grafmiller}

\seealso{
\code{\link[MuMIn]{AICc}} in \code{\link{MuMIn}}; \code{\link[Hmisc]{somers2}} in \code{\link{Hmisc}}
}

\references{
Burnham, K. P. and Anderson, D. R. (2002) \emph{Model selection and multimodel inference: a practical information-theoretic approach}. 2nd ed. New York, Springer-Verlag.
}

\examples{
## Not run: 
data(brown_genitives)

require(lme4)
require(languageR)
   
genitive.glmm = glmer(Type ~ PossrAnimacyBin + 
   PossrWordC + Genre + (1|Text), data = brown_genitives, 
   family = binomial, control = glmerControl(optimizer = "bobyqa"))

genitive.glmm.tab <- permute.varimp{genitive.glmm, brown_genitives}
## End(Not run)
}