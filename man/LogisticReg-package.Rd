\name{LogisticReg-package}
\alias{LogisticReg-package}
\alias{LogisticReg}
\docType{package}
\title{
\packageTitle{LogisticReg}
}
\description{
\packageDescription{LogisticReg}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{LogisticReg}
\packageIndices{LogisticReg}

}
\author{
\packageAuthor{LogisticReg}

Maintainer: \packageMaintainer{LogisticReg}
}

\examples{
set.seed(1)
y = sample(c(0, 1), 10, replace = TRUE)
 X = round(matrix(rnorm(30), 10, 3), 3)
res = lrm(y, X)
res = lrm(y, X, intercept = FALSE)
}