# self-defined functions for CDA on airline customer satisfaction
# credit to Thompson, R (and S-PLUS) Manual to Accompany Agresti’s Categorical Data Analysis (2002) 2nd edition

Wald.ci<-function(Table, aff.response, alpha=.05){
  # Gives two-sided Wald CI's for odds ratio, difference in proportions and relative risk.
  # Table is a 2x2 table of counts with rows giving the treatment populations
  # aff.response is a string like "c(1,1)" giving the cell of the beneficial response and the
  # treatment category
  # alpha is significance level
  pow<-function(x, a=-1) x^a
  z.alpha<-qnorm(1-alpha/2)
  if(is.character(aff.response))
    where<-eval(parse(text=aff.response))
  else where<-aff.response
  Next<-as.numeric(where==1) + 1
  # OR
  odds.ratio<-
    Table[where[1],where[2]]*Table[Next[1],Next[2]]/(Table[where[1],Next[2]]*Table[Next[1],where[
      2]])
  se.OR<-sqrt(sum(pow(Table)))
  ci.OR<-exp(log(odds.ratio) + c(-1,1)*z.alpha*se.OR)
  # difference of proportions
  p1<-Table[where[1],where[2]]/(n1<-Table[where[1],Next[2]] + Table[where[1],where[2]])
  p2<-Table[Next[1],where[2]]/(n2<-Table[Next[1],where[2]]+Table[Next[1],Next[2]])
  se.diff<-sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
  ci.diff<-(p1-p2) + c(-1,1)*z.alpha*se.diff
  # relative risk
  RR<-p1/p2
  se.RR<-sqrt((1-p1)/(p1*n1) + (1-p2)/(p2*n2))
  ci.RR<-exp(log(RR) + c(-1,1)*z.alpha*se.RR)
  list(OR=list(odds.ratio=odds.ratio, CI=ci.OR), proportion.difference=list(diff=p1-p2,
                                                                            CI=ci.diff), relative.risk=list(relative.risk=RR,CI=ci.RR))
}

Gamma.f<-function(x, pr=0.95)
{
  # x is a matrix of counts. You can use output of crosstabs or xtabs in R.
  # A matrix of counts can be formed from a data frame by using design.table.
  # Confidence interval calculation and output from Greg Rodd
  # Check for using S-PLUS and output is from crosstabs (needs >= S-PLUS 6.0)
  if(is.null(version$language) && inherits(x, "crosstabs")) { oldClass(x)<-NULL;
  attr(x, "marginals")<-NULL}
  n <- nrow(x)
  m <- ncol(x)
  pi.c<-pi.d<-matrix(0,nr=n,nc=m)
  row.x<-row(x)
  col.x<-col(x)
  for(i in 1:(n)){
    for(j in 1:(m)){
      pi.c[i, j]<-sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
      pi.d[i, j]<-sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
    }
  }
  C <- sum(pi.c*x)/2
  D <- sum(pi.d*x)/2
  psi<-2*(D*pi.c-C*pi.d)/(C+D)^2
  sigma2<-sum(x*psi^2)-sum(x*psi)^2
  gamma <- (C - D)/(C + D)
  pr2 <- 1 - (1 - pr)/2
  CIa <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
  list(gamma = gamma, C = C, D = D, sigma = sqrt(sigma2), Level = paste(
    100 * pr, "%", sep = ""), CI = paste(c("[", max(CIa[1], -1),
                                           ", ", min(CIa[2], 1), "]"), collapse = ""))
}

