##' Utility to subset trees by leaf outcome
##'
##' The purpose of this utility is to allow calculation of tree averages conditional on a specified outcome. This has particular usefulness if a tree is to calculate transitions for use in with other models (e.g. ODEs or Markov models)
##' 
##' @title Prune Trees by Outcome
##' @param TREE an input tree
##' @param outcome a string for a variable that can be interpreted as >0 on positive outcome leaves
##' @param negate whether the indicator should be negated
##' @return a pruned tree only containing routes to leaves with positive outcomes
##' @author Pete Dodd
##' @import data.tree
##' @export
PruneByOutcome <- function(TREE, outcome, negate = FALSE) {
  newtree <- data.tree::Clone(TREE)
  newtree$Set(anyout = 0)
  if (negate) {
    newtree$Set(anyout = 1,
                filterFun = function(x) !(as.numeric(x[[outcome]]) > 0) & (length(x$children)==0)
                )
  } else {
    newtree$Set(anyout = 1,
                filterFun = function(x) (as.numeric(x[[outcome]]) > 0)  & (length(x$children)==0)
                )
  }
  newtree$Do(function(x) x$anyout <- data.tree::Aggregate(x, "anyout", sum), traversal = "post-order")
  data.tree::Prune(newtree, function(x) x$anyout > 0) # removes all subtrees with anyout==0
  newtree$Do(function(node) node$RemoveAttribute("anyout"))
  newtree
}
