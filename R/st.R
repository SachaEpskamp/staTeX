## No paranthesis version:
stNP <- function(x,...)
{
  st(x,parantheses=FALSE)
}

## Generic method:
st <- function (x, ...) {
   UseMethod("st", x)
}

## 'htest' method:
st.htest <- function(x,...)
{

### chisq.test() ###
   if (any(grepl("X-squared|chi-square",names(x$statistic))))
   {
     # Extract Statistics:
     stat <- x$statistic['X-squared']
     df <- x$parameter['df']
     pval <- x$p.value

     return(staTeX("\\\\chi^2",stat,pval,df,...))
   }

### cor.test() ###
   if (any(grepl("correlation",x$method)))
   {
     # Extract Statistics:
     stat <- x$statistic
     para <- x$parameter
     pval <- x$p.value
     estimate <- x$estimate
     method <- x$method
     if (any(grepl("Pearson", x$method)))
     {
       confidint <- x$conf.int
       res <- staTeX("cor",estimate,pval,c(stat, para),...)
     } else if (any(grepl("Spearman", x$method)))
     {
       res <- staTeX("rho",estimate,pval,para,...)
     } else if (any(grepl("Kendall", x$method)))
     {
       res <- staTeX("tau",estimate,pval,para,...)
     } else
     {
       res <- staTeX("cor",estimate,pval,para,...)
     }

     return(res)
   }

# If all else fails:
     stat <- x$statistic
     df <- x$parameter
     pval <- x$p.value

     return(staTeX(names(x$statistic),stat,pval,df,...))
 }

## 'aov' method:
st.aov <- function(x,...)
{
   return(st(anova(x),...))
}

## 'lm' method:
st.lm <- function(x,...)
{
    sum <- summary(x)
    stat <- sum$fstatistic['value']
    df1 <- sum$fstatistic['numdf']
    df2 <- sum$fstatistic['dendf']
    pval <- pf(stat,df1,df2,lower.tail=FALSE)

    return(staTeX("F",stat,pval,c(df1,df2),...))
 }

## 'anova' method:
st.anova <- function(x,...)
{
    n <- nrow(x)-1
    res <- character(n)
    names(res) <- rownames(x)[1:n]
    for (i in 1:n)
    {
      if ("num Df"%in%names(x) & "den Df"%in%names(x))
      {
        res[i] <- staTeX("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'num Df'],x[i,'den Df']),...)
      } else
      {
        res[i] <- staTeX("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'Df'],x[n+1,'Df']),...)
      }
    }
    return(res)
  }


## 'Anova.mlm' method:
st.Anova.mlm <- function(x,...)
{
    ### DATA EXTRACTION COPIED FROM car:::Anova.mlm
    test <- x$test
    repeated <- x$repeated
    ntests <- length(x$terms)
    tests <- matrix(NA, ntests, 4)
    if (!repeated) 
        SSPE.qr <- qr(x$SSPE)
    for (term in 1:ntests) {
        eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else SSPE.qr, 
            x$SSP[[term]]), symmetric = FALSE)$values)
        tests[term, 1:4] <- switch(test, Pillai = stats:::Pillai(eigs, 
            x$df[term], x$error.df), Wilks = stats:::Wilks(eigs, 
            x$df[term], x$error.df), `Hotelling-Lawley` = stats:::HL(eigs, 
            x$df[term], x$error.df), Roy = stats:::Roy(eigs, 
            x$df[term], x$error.df))
    }
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3], 
        tests[ok, 4], lower.tail = FALSE))
    rownames(tests) <- x$terms
    colnames(tests) <- c("Df", "test stat", "approx F", "num Df", 
        "den Df", "Pr(>F)")
    tests <- structure(as.data.frame(tests), heading = paste("\nType ", 
        x$type, if (repeated) 
            " Repeated Measures", " MANOVA Tests: ", test, " test statistic", 
        sep = ""), class = c("anova", "data.frame"))
    ### END CODE COPIED FROM car:::Anova.mlm
    
    n <- nrow(tests) - 1
    res <- character(n)
    names(res) <- rownames(tests)[-1]
    for (i in 1:n)
    {
      res[i] <- staTeX("F",tests[['approx F']][i+1], tests[['Pr(>F)']][i+1], c(tests[['num Df']][i+1],tests[['den Df']][i+1]), ...)
    }
    return(res)
}



## Default method (simply an error)
st.default <- function(x,...) stop("The class of your object is not yet supported by st.\n\nPlease contact me (sacha.epskamp@gmail.com) with information on your object.")
