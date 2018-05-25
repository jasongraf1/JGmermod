\name{kappa.mer}
\alias{kappa.mer}
\title{Compute or Estimate the Condition Number of a MerMod Data Matrix}
\usage{
kappa.mer(fit)
}

\description{
Calculates the condition number of the data matrix obtained from a MerMod object
using the method in \code{\link{kappa}}.
}

\arguments{
\item{fit}{ a object of class \code{lmerMod} or \code{glmerMod}.}
\item{scale}{ logical. Should columns be scaled?}
\item{center}{ logical. Should columns be centered around the mean?}
\item{add.intercept}{ logical. Should a column for the model intercept be added?}
\item{exact}{ logical.  Should the result be exact?}
}

\details{
An adaptation of \code{\link{kappa}} from base R for use with mixed models from the \code{\link{lme4}} package.
See \code{help(kappa)} for details.
}

\value{
The condition number, \emph{kappa}, or an approximation if \code{exact = FALSE}.
}

\author{F. J. Tweedie, R. H. Baayen, J. Grafmiller}

\seealso{
\code{\link[languageR]{collin.fnc}}, \code{\link{kappa}}
}

\examples{
## Not run:
data(brown_genitives)

require(lme4)
require(languageR)

genitive.glmm = glmer(Type ~ PossrAnimacyBin +
   PossrWordC + Genre + (1|Text), data = brown_genitives,
   family = binomial, control = glmerControl(optimizer = "bobyqa"))

collin.fnc.mer(genitive.glmm)$cnumber
## End(Not run)
}