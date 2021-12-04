set.seed(1)
y = sample(c(0,1), 10, replace = T)
X = round(matrix(rnorm(30, 0, 10), 10, 3), 3)
colnames(X) = c("A","B","C")
Xnew = round(matrix(rnorm(15, 0, 10), 5, 3), 3)
colnames(Xnew) = c("A","B","C")

test_that("lrm works: y binary", {
  y_new = y
  y_new[3] = 2
  expect_error(lrm(y_new, X))
})


test_that("lrm works: y to numeric", {
  y_new = as.factor(y)
  expect_equal(as.vector(lrm(y_new, X)$dat$y), y)
})


test_that("lrm works: y X same length", {
  expect_error(lrm(c(y,1), X))
})

test_that("lrm works: coefficient", {
  lrm.coefs = lrm(y, X)[["coef"]]
  glm.coefs = glm(y ~ X, family = binomial, start=numeric(ncol(X)+1))[["coefficients"]]
  expect_equal(as.vector(lrm.coefs), as.vector(glm.coefs))
})

test_that("lrm works: fitted probabilities", {
  lrm.fitted = lrm(y, X)[["prob"]]
  glm.fitted = glm(y ~ X, family = binomial, start=numeric(ncol(X)+1))[["fitted.values"]]
  expect_equal(as.vector(lrm.fitted), as.vector(glm.fitted))
})


test_that("lrm works: log likelihood", {
  lrm.loglik = lrm(y, X)$loglik
  glm.loglik = logLik(glm(y ~ X, family = binomial, start=numeric(ncol(X)+1)))
  expect_equal(as.vector(lrm.loglik), as.vector(glm.loglik))
})

test_that("lrm works: deviance", {
  lrm.dev = lrm(y, X)$deviance
  glm.dev = glm(y ~ X, family = binomial, start=numeric(ncol(X)+1))[["deviance"]]
  expect_equal(as.vector(lrm.dev), as.vector(glm.dev))
})

test_that("lrm works: aic", {
  lrm.aic = lrm(y, X)$aic
  glm.aic = glm(y ~ X, family = binomial, start=numeric(ncol(X)+1))[["aic"]]
  expect_equal(as.vector(lrm.aic), as.vector(glm.aic))
})

test_that("lrm rsqr works", {
  
  lrm_rsrqr1 = lrm.rsqr(lrm(y, X))[[1]]
  lrm_rsrqr2 = lrm.rsqr(lrm(y, X))[[2]]
  glm.fit1 = logLik(glm(y ~ X, family = binomial, start=numeric(ncol(X)+1)))
  glm.fit0 <- glm(y~1, family=binomial(link=logit), start=numeric(1))
  l0=as.numeric(logLik(glm.fit0))
  l1=as.numeric(logLik(glm.fit1))
  R2_cox=1-(exp(l0-l1))^{2/length(y)}
  R2_max=1-exp(l0)^{2/length(y)}
  max_adj_R2<-R2_cox/R2_max
  
  expect_equal(as.vector(lrm_rsrqr1), as.vector(R2_cox))
  expect_equal(as.vector(lrm_rsrqr2), as.vector(max_adj_R2))
})

test_that("lrm.predict works", {
  lrm.fit = lrm(y, X)
  glm.fit = glm(y ~ A+B+C, data=data.frame(X), family = binomial, start=numeric(ncol(X)+1))
  lrm.p = lrm.predict(Xnew = Xnew, lrm.fit)[,1]
  glm.p = predict(glm.fit, data.frame(Xnew),type = "response")
  expect_equal(as.vector(lrm.p), as.vector(glm.p))
  expect_error(lrm.predict(Xnew = cbind(Xnew,rnorm(5)), lrm.fit))
})

