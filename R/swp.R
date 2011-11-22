##' Return statistic and p values in LaTeX format
##' 
##' This is the function used by \code{\link{swst}} to generate the LaTeX code.
##' 
##' 
##' @param statistic Name of the statistic used
##' @param value Value of the statistic
##' @param pval p-value
##' @param df The degrees of freedom. Can be a vector with multiple degrees of
##' freedom
##' @param digits The number of digits to be printed
##' @param sig Logical, if TRUE not the real value but only the value compared
##' to levels of 'alpha' is printed. This means that if p is smaller than one of
##' the levels of 'alpha' than that is returned (e.g. p<0.05) else ns is
##' returned.
##' @param alpha A vector specifying the significance levels used by 'sig'
##' @param parantheses Logical, should parantheses be printed?
##' @author Sacha Epskamp
##' @seealso \code{\link{swst}} \code{\link{swstNP}}
##' @export
swp <- function(statistic,value,pval,df=NULL,digits=3,sig=FALSE,alpha=c(0.05,0.01,0.001),parantheses=TRUE)
{
  po <- ifelse(parantheses,"(","")
  pc <- ifelse(parantheses,")","")  
  
  if (sig)
  {
    if (pval > max(alpha))
    {
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        "=",
        format(round(value,digits),scientific=FALSE),
        "$, ns",pc,sep="")
    } else
    {
      whichsig <- which(alpha == min(alpha[pval<=alpha]))
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        "=",
        format(round(value,digits),scientific=FALSE),
        "$, $p<",
        format(alpha[whichsig],scientific=FALSE),
        "$",pc,sep="")
    } 
  } else
  {
    if (pval > 10^(-1*digits))
    {
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        "=",
        format(round(value,digits),scientific=FALSE),
        "$, $p=",
        format(round(pval,digits),scientific=FALSE),
        "$",pc,sep="")
    } else
    {
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        "=",
        format(round(value,digits),scientific=FALSE),
        "$, $p<",
        format(round(10^(-1*digits),digits),scientific=FALSE),
        "$",pc,sep="")    
    }
  }
  return(res)  
}
