### No parantheses version:


##' LaTeX code for statistical reference
##'
##' The same as \code{\link{swst}} except that no parantheses are printed
##'
##' This function attemps to extract information needed to make a LaTeX code of
##' the statistic in the following format (for an F value):
##'
##' (F(df1,df2)=..., p=...)
##'
##' Some arguments can be passed to \code{\link{swp}} to modify the way the
##' statistic is printed. For example, 'digits' an be used to change the amount
##' of digits printed and 'sig' can be used to change the way to show only
##' significance cutoff scores. For example, with p=0.02 this would return:
##' (F(df1,df2)=..., p<0.05)
##'
##' This is aimed to facilitate the use of Sweave especially for users new to R.
##' This is currently in development and is so far only a few commonly used
##' statistics are implemented, but this should change in future versions.
##'
##' If you have any ideas on other ways to facilitate using Sweave (except of
##' course existing methods such as xtable) please contact me. Also, if you know
##' of an object that is not implemented please contact me as well. If you
##' supply me a small piece of code that extracts the statistic, degrees of
##' freedom and p-value from that object I will include you in the author list.
##'
##' @param x An input object. Currently supported are a numerical vector and
##' objects of classes "htest" (depending on statistic), "lm", "aov" and
##' "anova".
##' @param \dots Arguments sent to \code{\link{swp}}
##' @return A strings containing LaTeX code, or a named vector of strings
##' containing LaTeX code
##' @author Sacha Epskamp (sacha.epskamp@@gmail.com)
##' @seealso \code{\link{swst}} \code{\link{swp}}
##' @export swstNP
##' @examples
##'
##' ### Using examples from documentation of the used functions
##' # t-test:
##' require(graphics)
##' tTest <- t.test(1:10,y=c(7:20))
##'
##' swst(tTest)
##'
##' # Significance:
##' swst(tTest,sig=TRUE)
##'
##' tTest <- t.test(1:10,y=1:10)
##' swst(tTest,sig=TRUE)
##'
##' # Correlation test:
##' ## Hollander & Wolfe (1973), p. 187f.
##' ## Assessment of tuna quality.  We compare the Hunter L measure of
##' ##  lightness to the averages of consumer panel scores (recoded as
##' ##  integer values from 1 to 6 and averaged over 80 such values) in
##' ##  9 lots of canned tuna.
##'
##' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
##' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
##'
##' ##  The alternative hypothesis of interest is that the
##' ##  Hunter L value is positively associated with the panel score.
##' corTest <- cor.test(x, y, method = "kendall", alternative = "greater")
##'
##' swst(corTest)
##'
##' # Chi-square test:
##' M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
##' dimnames(M) <- list(gender=c("M","F"),
##'                     party=c("Democrat","Independent", "Republican"))
##' chisqTest <- chisq.test(M)
##'
##' swst(chisqTest)
##'
##' # Linear model:
##' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
##' ## Page 9: Plant Weight Data.
##' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
##' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
##' group <- gl(2,10,20, labels=c("Ctl","Trt"))
##' weight <- c(ctl, trt)
##' lm.D9 <- lm(weight ~ group)
##' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
##'
##' swst(lm.D9)
##' swst(lm.D90)
##'
##' # ANOVA:
##' ## From Venables and Ripley (2002) p.165.
##' utils::data(npk, package="MASS")
##'
##' ## Set orthogonal contrasts.
##' op <- options(contrasts=c("contr.helmert", "contr.poly"))
##' npk.aov <- aov(yield ~ block + N*P*K, npk)
##'
##' swst(npk.aov)
##'
##' @rdname swstNP
swstNP <- function(x,...)
{
  swst(x,parantheses=FALSE)
}

### Generic method ###

##' @title swst:LaTeX code for statistical reference
##'
##' This function attempts to extract information on statistic, degrees of
##' freedom and p-value from a given object, and returns a LaTeX code.
##'
##' This function attemps to extract information needed to make a LaTeX code of
##' the statistic in the following format (for an F value):
##'
##' (F(df1,df2)=..., p=...)
##'
##' Some arguments can be passed to \code{\link{swp}} to modify the way the
##' statistic is printed. For example, 'digits' an be used to change the amount
##' of digits printed and 'sig' can be used to change the way to show only
##' significance cutoff scores. For example, with p=0.02 this would return:
##' (F(df1,df2)=..., p<0.05)
##'
##' This is aimed to facilitate the use of Sweave especially for users new to R.
##' This is currently in development and is so far only a few commonly used
##' statistics are implemented, but this should change in future versions.
##'
##' If you have any ideas on other ways to facilitate using Sweave (except of
##' course existing methods such as xtable) please contact me. Also, if you know
##' of an object that is not implemented please contact me as well. If you
##' supply me a small piece of code that extracts the statistic, degrees of
##' freedom and p-value from that object I will include you in the author list.
##' 
##' @aliases swst swst.htest swst.aov swst.lm swst.anova
##' @param x An input object. Currently supported are a numerical vector and
##' objects of classes "htest" (depending on statistic), "lm", "aov" and
##' "anova".
##' @param \dots Arguments sent to \code{\link{swp}}
##' @return A strings containing LaTeX code, or a named vector of strings
##' containing LaTeX code
##' @author Sacha Epskamp (sacha.epskamp@@gmail.com)
##' @seealso \code{\link{swp}} \code{\link{swstNP}}
##' @examples
##' 
##' ### Using examples from documentation of the used functions
##' # t-test:
##' require(graphics)
##' tTest <- t.test(1:10,y=c(7:20))
##' 
##' swst(tTest)
##' 
##' # Significance:
##' swst(tTest,sig=TRUE)
##' 
##' tTest <- t.test(1:10,y=1:10)
##' swst(tTest,sig=TRUE)
##' 
##' # Correlation test:
##' ## Hollander & Wolfe (1973), p. 187f.
##' ## Assessment of tuna quality.  We compare the Hunter L measure of
##' ##  lightness to the averages of consumer panel scores (recoded as
##' ##  integer values from 1 to 6 and averaged over 80 such values) in
##' ##  9 lots of canned tuna.
##' 
##' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
##' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
##' 
##' ##  The alternative hypothesis of interest is that the
##' ##  Hunter L value is positively associated with the panel score.
##' corTest <- cor.test(x, y, method = "kendall", alternative = "greater")
##' 
##' swst(corTest)
##' 
##' # Chi-square test:
##' M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
##' dimnames(M) <- list(gender=c("M","F"),
##'                     party=c("Democrat","Independent", "Republican"))
##' chisqTest <- chisq.test(M)
##' 
##' swst(chisqTest)
##' 
##' # Linear model:
##' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
##' ## Page 9: Plant Weight Data.
##' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
##' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
##' group <- gl(2,10,20, labels=c("Ctl","Trt"))
##' weight <- c(ctl, trt)
##' lm.D9 <- lm(weight ~ group)
##' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
##' 
##' swst(lm.D9)
##' swst(lm.D90)
##' 
##' # ANOVA:
##' ## From Venables and Ripley (2002) p.165.
##' utils::data(npk, package="MASS")
##' 
##' ## Set orthogonal contrasts.
##' op <- options(contrasts=c("contr.helmert", "contr.poly"))
##' npk.aov <- aov(yield ~ block + N*P*K, npk)
##' 
##' swst(npk.aov)
##' 
##' @rdname swst
##' @export swst
swst <- function (x, ...) {
   UseMethod("swst", x)
}

##' @return \code{NULL}
##'
##' @title swst:LaTeX code for statistical reference
##' @rdname swst
##' @method swst htest
##' @S3method swst htest
swst.htest <- function(x,...)
{

### chisq.test() ###
   if (any(grepl("X-squared|chi-square",names(x$statistic))))
   {
     # Extract Statistics:
     stat <- x$statistic['X-squared']
     df <- x$parameter['df']
     pval <- x$p.value

     return(swp("\\\\chi^2",stat,pval,df,...))
   }


# If all else fails:
     stat <- x$statistic
     df <- x$parameter
     pval <- x$p.value

     return(swp(names(x$statistic),stat,pval,df,...))
 }

##' @return \code{NULL}
##'
##' @title swst:LaTeX code for statistical reference
##' @rdname swst
##' @method swst aov
##' @S3method swst aov
swst.aov <- function(x,...)
{
   return(swst(anova(x),...))
}

##' @return \code{NULL}
##'
##' @title swst:LaTeX code for statistical reference
##' @rdname swst
##' @method swst lm
##' @S3method swst lm
swst.lm <- function(x,...)
{
    sum <- summary(x)
    stat <- sum$fstatistic['value']
    df1 <- sum$fstatistic['numdf']
    df2 <- sum$fstatistic['dendf']
    pval <- pf(stat,df1,df2,lower.tail=FALSE)

    return(swp("F",stat,pval,c(df1,df2),...))
 }

##' @return \code{NULL}
##'
##' @title swst:LaTeX code for statistical reference
##' @rdname swst
##' @method swst anova
##' @S3method swst anova
swst.anova <- function(x,...)
{
    n <- nrow(x)-1
    res <- character(n)
    names(res) <- rownames(x)[1:n]
    for (i in 1:n)
    {
      if ("num Df"%in%names(x) & "den Df"%in%names(x))
      {
        res[i] <- swp("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'num Df'],x[i,'den Df']),...)
      } else
      {
        res[i] <- swp("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'Df'],x[n+1,'Df']),...)
      }
    }
    return(res)
  }

##' @return \code{NULL}
##' 
##' @title swst:LaTeX code for statistical reference
##' @rdname swst
##' @method swst default
##' @S3method swst default
swst.default <- function(x,...) stop("The class of your object is not yet supported by swst.\n\nPlease contact me (sacha.epskamp@gmail.com) with information on your object.")
