#"Copyright© 2018 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA."
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
#
#' EvaluateRules
#'
#' Evaluates the selected Nelson rules for a given numerical vector.
#' @param x Series to be evaluated, numerical vector
#' @param type Type of control chart, either "i" for i-chart (default) or "c" for c-chart
#' @param whichRules Selection of Nelson rules beeing evaluated, vector with numbers from 1 to 8
#' @param lcl Lower control limit, single numeric value (expected as mean - controlLimitDistance * sigma), if missing the function calculates it from data
#' @param cl Central line, single numeric value (expected as mean), if missing the function calculates it from data
#' @param  ucl Upper control limit, single numeric value (expected as mean + controlLimitDistance * sigma), if missing the function calculates it from data
#' @param controlLimitDistance Multiple of st.dev to be used to calculate limits, possible values: 1, 2, 3 (default); this parameter affect the interpretation of lcl and ucl parameters
#' @param returnAllSelectedRules Resulting dataframe will contain all selected rules, either True or False, if missing only valid rules returned
#' @param  parRules Optional parameters for specific rules, for details see \code{\link{SetParameters}}
#' @return Dataframe containing original vector and rules evaluation
#' @importFrom stats na.omit sd
#' @examples
#' # Evaluate data, use all 8 Nelson rules, limits are specified by user
#' EvaluateRules(x = rnorm(10), whichRules = 1:8, lcl = 0, cl = 50, ucl = 100)
#' #Evaluate only rule 1, 3, 5, calculate limits from data using c-chart formula,
#' #use 2 sigma instead of 3, modify default behaviour of rule by pars variable
#' #created by function SetParameters()
#' pars = SetParameters()
#' EvaluateRules(x = rpois(10, lambda = 15), type = 'c', whichRules = c(1,3,5), lcl = NA, cl = NA,
#' ucl = NA, controlLimitDistance = 2, parRules = pars)
#' # pars is object of optional parameters created by SetParameters() function
#' @details
#' # Only Rules 1-4 relevant for c-chart.\cr
#' # Check for non negative data for c-chart.\cr
#' # For controlLimitDistance less than or equal to 2 disable rule 5.\cr
#' # For controlLimitDistance less than or equal to 1 disable rule 5,6,8.\cr
#' # For returnAllSelectedRules=TRUE columns of invalid rules for given evaluation are filled with NAs.\cr
#' @export
EvaluateRules <- function(x, type='i', whichRules=1:8, lcl=NA, cl=NA, ucl=NA, controlLimitDistance=3, returnAllSelectedRules=F, parRules=NULL){

    ## Checking X:
    if(missing(x))
    {stop("argument 'x' is missing, with no default")}
    # Check dims
    if(!is.vector(x))
    {stop("x must be vector!")}
    # Check type
    if(mode(x) != 'numeric')
    {stop("column x is not numeric!")}
    # Check length
    if(length(na.omit(x))<=1)
    {stop("vector x is to short!")}

    ## Checking WHICHRULES:
    # Check dims
    if(!is.vector(whichRules))
    {stop("whichRules must be vector!")}
    # Check length
    if(length(whichRules) < 1)
    {stop('At least one rule must be specified')}
    # Check values
    if(!is.numeric(whichRules))
    {stop("whichRules is not numeric!")}
    if(!all(whichRules %in% 1:8))
    {stop('whichRules must contation integers in range 1..8')}
    SelectedRules = whichRules

    ## Checking TYPE:
    # Check dims
    if(length(type)>1)
    {stop("type must be single character!")}
    # Check value
    if(!(type %in% c('i','c')))
    {stop('wrong type of chart')}
    # Check only non negative values for c-chart
    if(type=="c" & any(x<0, na.rm = T))
    {stop("count data can not contain negative values")}
    # only rules 1 to 4 are valid for c-chart
    if(type=="c" &  !any(1:4 %in% whichRules))
    {stop("At least one rule from 1 to 4 must be specified for c-chart")}
    # select only valid rules for c-chart
    if(type=="c"){
        remove = c(5,6,7,8)
        whichRules = whichRules[! whichRules %in% remove]
    }

    ## Checking LCL,CL,UCL
    # Check dims
    if(length(lcl)>1 |length(cl)>1| length(ucl)>1)
    {stop("limits lcl, cl and ucl must contain only single value")}
    # Check values (numeric or NA)
    if(!(is.na(lcl) | is.numeric(lcl)))
    {stop("parameter lcl must contain only numeric value or NA")}
    if(!(is.na(cl) | is.numeric(cl)))
    {stop("parameter cl must contain only numeric value or NA")}
    if(!(is.na(ucl) | is.numeric(ucl)))
    {stop("parameter ucl must contain only numeric value or NA")}

    ## Checking controlLimitDistance
    # Check dims
    if(length(controlLimitDistance)>1)
    {stop("controlLimitDistance must be a single value")}
    # check not NA
    if(is.na(controlLimitDistance)){stop('parameter must be only 1,2 or 3 not NA')}
    # Check value (only 1,2,3)
    if(!any(controlLimitDistance == c(1,2,3))){stop('parameter must be only 1,2 or 3')}
    # for controlLimitDistance less than or equal to 2 disable rule 5
    if(controlLimitDistance <= 2){
        remove = c(5)
        whichRules = whichRules[! whichRules %in% remove]
    }
    # for controlLimitDistance less than or equal to 1 disable rule 5,6,8
    if(controlLimitDistance <= 1){
        remove = c(5,6,8)
        whichRules = whichRules[! whichRules %in% remove]
    }

    ## Checking returnAllSelectedRules
    # check dims
    if(length(returnAllSelectedRules)>1)
    {stop("returnAllSelectedRules must be a single value")}
    # Check value (Logical not NA)
    if(!(is.logical(returnAllSelectedRules) & !is.na(returnAllSelectedRules))){stop('parameter must be logical, either TRUE or FALSE')}

    ## Checking PARRULES
    # check structure
    # if NULL - optional parameters ParRules are set to default defined by function Setparameters()
    if(is.null(parRules)){
        parRules = SetParameters()
    }
    namesDefault = lapply(SetParameters(), lapply, names)
    namesActual = lapply(parRules, lapply, names)
    if(!isTRUE(all.equal(namesDefault, namesActual))){stop('error in structure of parRules')}
    # Checks one to one list of names of parRules whether it wasn't mistakenly changed during manual parameter configuration
    # added isTRUE bacause all.equal does not return logical value when two objects are not equal
    # check convention
    if(parRules$Rule3$convention != "minitab" & parRules$Rule3$convention != "jmp")
    {stop("parameter convention in Rule3 does not match 'minitab' or 'jmp'")}
    if(parRules$Rule4$convention != 'minitab'& parRules$Rule4$convention != 'jmp')
    {stop("parameter convention in Rule4 does not match 'minitab' or 'jmp'")}
    if(parRules$Rule3$equalBreaksSeries != T & parRules$Rule3$equalBreaksSeries != F)
    {stop('parameter equalBreaksSeries in Rule3 is not logical')}
    # recode convention (string) to a number
    if(parRules$Rule3$convention == 'minitab'){parRules$Rule3$convention = 1}
    else{parRules$Rule3$convention = 2}
    if(parRules$Rule4$convention == 'minitab'){parRules$Rule4$convention = 1}
    else{parRules$Rule4$convention = 2}
    if(parRules$Rule3$equalBreaksSeries == T){parRules$Rule3$equalBreaksSeries = 1}
    else{parRules$Rule3$equalBreaksSeries = 2}
    # check sides
    if(parRules$Rule1$sides != "two-sided" & parRules$Rule1$sides != "upper" & parRules$Rule1$sides != "lower")
    {stop("parameter sides in Rule1 does not match 'two-sided', 'upper' or 'lower'")}
    # recode sides (string) to a number
    if(parRules$Rule1$sides == 'two-sided'){parRules$Rule1$sides = 1}
    if(parRules$Rule1$sides == 'upper'){parRules$Rule1$sides = 2}
    if(parRules$Rule1$sides == 'lower'){parRules$Rule1$sides = 3}
    # Check length
    lapply(parRules, lapply, function(x){if(length(x)!=1){stop('parameter must contain only a single value')}})
    # Check values (numeric)
    lapply(parRules, lapply, function(x){if(!(is.numeric(x) & x%%1==0 & x > 0)){stop('parameter must be a positive integer')}})

    ## Calculations of limits and zones
    # Limits for zone calculation
    limits = CalculateLimits(x, lcl, cl, ucl, type, controlLimitDistance)
    # Calculate zones
    zoneB = CalculateZoneBorders(limits, controlLimitDistance)
    # result is a dataframe cointaining original vector x
    res = as.data.frame(x)

    ## Evaluation of rules
    for(i in whichRules){
        ruleName <- paste("Rule",as.character(i),sep="")
        # when the rule has optional parameters object parRules is passed to function NelsonRules
        if(ruleName %in% names(parRules)){
            res[ruleName] <- NelsonRules(ruleN=ruleName, data=x, zoneB=zoneB, limits = limits, parRules=parRules[[ruleName]])
        }
        else{
            res[ruleName] <- NelsonRules(ruleN=ruleName, data=x, zoneB=zoneB, limits = limits)
        }
    }
    if(returnAllSelectedRules == TRUE){
        diff = setdiff(SelectedRules, whichRules)
        for(i in diff){
            ruleName <- paste("Rule",as.character(i),sep="")
            res[ruleName] <- rep(NA, times = nrow(res))
        }

    }
    return(res)
}

################################
#  functions for calculations  #
################################

#' CalculateLimits
#'
#' Evaluates whether to use custom limits or calculate them from the data.
#' @param x Numerical vector
#' @param lcl Lower control limit, single value or NA
#' @param  cl Central line, single value or NA
#' @param ucl Upper control limit, single value or NA
#' @param type Type of control chart, either "i" for i-chart (default) or "c" for c-chart
#' @param controlLimitDistance Multiple of st.dev to be used to calculate limits, possible values: 1, 2, 3 (default); this parameter affect the interpretation of lcl and ucl parameters
#' @return Named list with limits
#' @details If at least two limits were provided, the missing ones are calculated from the them. If one or zero limits were provided the rest is computed from data.
#' @examples
#' CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
#' @export
CalculateLimits <- function(x, lcl=NA, cl=NA, ucl=NA, type="i", controlLimitDistance=3){

    # LCL, CL and UCL provided
    if(is.numeric(lcl) & is.numeric(cl) & is.numeric(ucl)){
        if(!(lcl <= cl & cl <= ucl))
        {stop("parameters must be in form lcl<=cl<=ucl")}
        return(list(ucl=ucl, cl=cl, lcl=lcl))
    }
    # None of the limits provided
    if(is.na(lcl) & is.na(cl) & is.na(ucl)){

        if(type == 'i'){
            ucl = mean(x, na.rm = T) + controlLimitDistance*sd(x, na.rm = T)
            cl = mean(x, na.rm = T)
            lcl = mean(x, na.rm = T) - controlLimitDistance*sd(x, na.rm = T)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            ucl = mean(x, na.rm = T) + controlLimitDistance*sqrt(mean(x, na.rm = T))
            cl = mean(x, na.rm = T)
            lcl = mean(x, na.rm = T) - controlLimitDistance*sqrt(mean(x, na.rm = T))
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
    # CL and UCL provided
    if(is.na(lcl) & is.numeric(cl) & is.numeric(ucl)){
        if(!(cl <= ucl))
        {stop("parameters must be in form cl<=ucl")}

        if(type == 'i'){
            lcl = cl - (ucl - cl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            lcl = cl - (ucl - cl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
    # CL and LCL provided
    if(is.na(ucl) & is.numeric(cl) & is.numeric(lcl)){
        if(!(lcl <= cl))
        {stop("parameters must be in form lcl<=cl")}

        if(type == 'i'){
            ucl = cl + (cl - lcl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            ucl = cl + (cl - lcl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
    # LCL and UCL provided
    if(is.na(cl) & is.numeric(lcl) & is.numeric(ucl)){
        if(!(lcl <= ucl))
        {stop("parameters must be in form lcl<=ucl")}

        if(type == 'i'){
            cl = lcl + (ucl - lcl)/2
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            cl = lcl + (ucl - lcl)/2
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
    # LCL provided
    if(is.na(cl) & is.na(ucl) & is.numeric(lcl)){
        if(type == 'i'){
            cl = mean(x, na.rm = T)
            ucl = cl + (cl - lcl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            cl = mean(x, na.rm = T)
            ucl = cl + (cl - lcl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
    # CL provided
    if(is.na(lcl) & is.na(ucl) & is.numeric(cl)){
        if(type == 'i'){
            ucl = cl + controlLimitDistance*sd(x, na.rm = T)
            lcl = cl - (ucl - cl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            ucl = cl + controlLimitDistance*sqrt(cl)
            lcl = cl - (ucl - cl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
    # UCL provided
    if(is.na(cl) & is.na(lcl) & is.numeric(ucl)){
        if(type == 'i'){
            cl = mean(x, na.rm = T)
            lcl = cl - (ucl - cl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
        if(type == 'c'){
            cl = mean(x, na.rm = T)
            lcl = cl - (ucl - cl)
            return(list(ucl=ucl, cl=cl, lcl=lcl))
        }
    }
}

#' CalculateZoneBorders
#'
#' Some Nelson rules uses so-called zones. This function calculates the borders of the zones for given limits.
#' @param limits List of limits provided by \code{\link{CalculateLimits}}
#' @param controlLimitDistance Multiple of st.dev to be used to calculate limits, possible values: 1, 2, 3 (default); this parameter affect the interpretation of lcl and ucl parameters
#' @return Vector of zones
#' @examples
#' limits = CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
#' CalculateZoneBorders(limits)
#' #limits is object created by CalculateLimits() function
#' @export
CalculateZoneBorders <- function(limits, controlLimitDistance=3){
    # zones are calculated using only upper and lower control limit
    zoneB = (-controlLimitDistance:controlLimitDistance) * (limits$ucl - limits$lcl)/(2*controlLimitDistance) + (limits$ucl+limits$lcl)/2
    names(zoneB) = as.character(c(-controlLimitDistance:controlLimitDistance))
    return(zoneB)
}

#' SetParameters
#'
#' Creates optional parameters with default settings.
#' @return List of optional parameters
#' @details The function is called without any parameter. If you want to modify any or the rules' setting, modify the result of this function and plug it to \code{\link{EvaluateRules}}'s parRules parameter.
#' @examples
#' pars <- SetParameters()
#' pars$Rule1$sides <- "upper"
#' #function doos not need any input parameters
#' @export
SetParameters <- function(){
    parRules = list('Rule1' = list(sides='two-sided'),
                    'Rule2' = list(nPoints=9),
                    'Rule3' = list(nPoints=6, convention = 'jmp', equalBreaksSeries = T),
                    'Rule4' = list(nPoints=14, convention = 'jmp'),
                    'Rule5' = list(minNPoints=2, nPoints=3),
                    'Rule6' = list(minNPoints=4, nPoints=5),
                    'Rule7' = list(nPoints=15),
                    'Rule8' = list(nPoints=8))
}

###################
#  Nelson rules  #
##################

#' NelsonRules
#'
#' Auxiliary function to calling individual Rule functions.
#' @param ruleN Name of individual Rule function "Rule1" to "Rule8"
#' @param data Data to be checked, numerical vector
#' @param zoneB Vector of zones created by \code{\link{CalculateLimits}}
#' @param limits List of limit created by \code{\link{CalculateLimits}}
#' @param parRules List of optional parameters for this particular rule
#' @param ... unspecified arguments of a function
#' @return Result of individual Rule function with predefined parameters
#' @details Handling the missing values:
#'
#' Missing values are represented by the symbol NA - not available.
#'
#' Rule 1:
#'  NAs do not violate this rule.
#'
#' Rule 2-8:
#'   NAs are ignored, they do not break Rule evaluation.
#'   NA values are removed from the vector, the rule function is calculated
#'   and then the NAs are returned back to it's original position in the vector.
#' @export
NelsonRules <- function(ruleN, data, zoneB, limits, parRules=NULL, ...){
    switch(ruleN,
           'Rule1'={
               return(Rule1(x = data, lcl = limits$lcl, ucl = limits$ucl, sides = parRules$sides))
           },
           'Rule2'={
               return(Rule2(x = data, cl = limits$cl, nPoints = parRules$nPoints))
           },
           'Rule3'={
               return(Rule3(x = data, nPoints = parRules$nPoints, convention = parRules$convention, equalBreaksSeries = parRules$equalBreaksSeries))
           },
           'Rule4'={
               return(Rule4(x = data, nPoints = parRules$nPoints, convention = parRules$convention))
           },
           'Rule5'={
               if(!(parRules$minNPoints < parRules$nPoints)){stop('Rule5 - nPoints must be greater than minNPoints')}
               return(Rule5(x = data, zoneB = zoneB, minNPoints = parRules$minNPoints, nPoints = parRules$nPoints))
           },
           'Rule6'={
               if(!(parRules$minNPoints < parRules$nPoints)){stop('Rule6 - nPoints must be greater than minNPoints')}
               return(Rule6(x = data, zoneB = zoneB, minNPoints = parRules$minNPoints, nPoints = parRules$nPoints))
           },
           'Rule7'={
               return(Rule7(x = data, zoneB = zoneB, nPoints = parRules$nPoints))
           },
           'Rule8'={
               return(Rule8(x = data, zoneB = zoneB, nPoints = parRules$nPoints))
           }

    )
}

#' Rule 1
#'
#' One point beyond the control limits
#' @param x Numerical vector
#' @param lcl Lower control limit, single number
#' @param ucl Upper control limit, single number
#' @param ... unspecified arguments of a function
#' @param sides Monitored side of the process: either "two-sided" (default), "upper" or "lower"
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' inequality used during evaluation
#'
#' parametr sides is internally encoded as: 1 for "two-sided", 2 for "upper", 3 for "lower"
#' @examples
#' Rule1(x = rnorm(10), lcl = 10, ucl = 100, sides = "two-sided")
#' @export
Rule1 <- function(x, lcl, ucl, sides, ...) {
    if(sides==1)
    {
        Rule1 <- ifelse(is.na(x),0,ifelse(x > ucl | x < lcl, 1,0))
        return(Rule1)
    }
    if(sides==2)
    {
        Rule1 <- ifelse(is.na(x),0,ifelse(x > ucl, 1,0))
        return(Rule1)
    }
    if(sides==3)
    {
        Rule1 <- ifelse(is.na(x),0,ifelse(x < lcl, 1,0))
        return(Rule1)
    }
}

#' Rule 2
#'
#' Nine points in a row are on one side of the central line.
#' @param x Numerical vector
#' @param cl central line, single number
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' inequality used during evaluation
#' @examples
#' Rule2(x = rnorm(20), cl=0, nPoints = 9)
#' @export
Rule2 <- function(x, cl, nPoints=9, ...){
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len = length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    xZoned <- ifelse(x > cl, 1, ifelse(x < cl, -1, 0))
    violation <- rep(0, times = len)
    for(i in nPoints:len ){
        temp <- xZoned[(i-nPoints+1):i]
        violation[i] = ifelse(sum(temp == 1) >= nPoints | sum(temp == -1) >= nPoints, 1, 0)
    }
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}

#' Rule 3
#'
#' Six points in a row steadily increasing or decreasing.
#' @param x Numerical vector
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param convention Calculation according to 'minitab' or 'jmp' (see details)
#' @param equalBreaksSeries Equal values break consequtive series of points
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' parameter equalBreakSeries is internally encoded as: 1 for TRUE and 2 for FALSE \cr
#'
#' parameter convention is internally encoded as: 1 for 'minitab' and 2 for 'jmp'
#'
#' Difference in convention parameter is as follows: \cr
#' 'minitab' - seven points in a row steadily increasing or decreasing \cr
#' 'jmp' - six points in a row steadily increasing or decreasing
#' @examples
#' Rule3(x = rnorm(20), nPoints = 6, convention = 1, equalBreaksSeries = 1)
#' @export
Rule3 <- function(x, nPoints=6, convention = 1, equalBreaksSeries = 1, ...){
    if(convention == 1){nPoints = nPoints + 1}
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len = length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    violation <- rep(0, times = len)
    for(i in nPoints:len ){
        temp <- x[(i - nPoints+1):i]
        if(equalBreaksSeries == 1){
            violation[i] = ifelse(all(diff(temp)>0) | all(diff(temp)<0),1,0)
        }
        else{
            violation[i] = ifelse(all(diff(temp)>=0) | all(diff(temp)<=0),1,0)
        }
    }
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}

#' Rule 4
#'
#' Fourteen or more points in a row alternate in direction, increasing then decreasing.
#
#' @param x Numerical vector
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param convention Calculation according to 'minitab' or 'jmp' (see details)
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#'parameter convention is internally encoded as: 1 for 'minitab' and 2 for 'jmp' \cr
#'
#' Difference in convention parameter is as follows: \cr
#' 'minitab' - 15 or more points (14 changes of direction) in a row alternate in direction, increasing then decreasing \cr
#' 'jmp' - 14 or more points (13 changes of direction) in a row alternate in direction, increasing then decreasing
#' @examples
#' Rule4(x = rnorm(20), nPoints = 14,convention = 1)
#' @export
Rule4 <- function(x, nPoints=14,convention = 1, ...){
    if(convention == 1){nPoints = nPoints + 1}
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len <- length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    signs <- sign(diff(x))[-len]
    signsDiff <- signs[1:(len-2)]*signs[2:(len-1)] == -1
    count <- signsDiff[1]
    for(i in 2:(len-2)){
        count[i] <- (count[i-1]+1) * signsDiff[i]
    }
    violation <- c(0,0,count>=(nPoints-2))
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}

#' Rule 5
#'
#' Two out of three consecutive points beyond the 2*sigma limits on same side of center line.
#' @param x Numerical vector
#' @param zoneB Vector of zone borders
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param minNPoints Minimal number of points in a sequence violating a rule
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' inequality used during evaluation \cr
#' Rule is violated also if the first two points are beyond the 2*sigma limits
#' During calculation of EvaluateRules function wiht controlLimitDistance <= 2, the evaluation of this rule is suppressed
#' @examples
#' limits = CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
#' zones = CalculateZoneBorders(limits)
#' Rule5(x = rnorm(20), zoneB = zones, minNPoints = 2, nPoints = 3)
#' #zones is object created by function CalculateZoneBorders()
#' @export
Rule5 <- function(x, zoneB, minNPoints=2, nPoints=3, ...){
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len = length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    upper <- zoneB["2"]
    lower <- zoneB["-2"]
    xZoned <- ifelse(x > upper, 1, ifelse(x < lower, -1, 0))
    violation <- rep(0, times = len)
    if(all(xZoned[1:minNPoints]==1)|all(xZoned[1:minNPoints]==-1)){violation[minNPoints]=1}
    for(i in nPoints:len ){
        temp <- xZoned[(i-nPoints+1):i]
        violation[i] = ifelse(((sum(temp == 1) >= minNPoints) & temp[length(temp)] == 1) | ((sum(temp == -1) >= minNPoints) & temp[length(temp)] == -1), 1, 0)
    }
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}

#' Rule 6
#'
#' Four or five out of five points in a row are more than 1 standard deviation from the mean in the same direction.
#' @param x Numerical vector
#' @param zoneB Vector of zone borders
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param minNPoints Minimal number of points in a sequence violating a rule
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' inequality used during evaluation
#' Rule is violated also if the first four points are beyond the 1 standard deviation from the mean
#' During calculation of EvaluateRules function wiht controlLimitDistance <= 1, the evaluation of this rule is suppressed
#' @examples
#' limits = CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
#' zones = CalculateZoneBorders(limits)
#' Rule6(x = rnorm(20), zoneB = zones, minNPoints = 4, nPoints = 5)
#' #zones is object created by function CalculateZoneBorders()
#' @export
Rule6 <- function(x, zoneB, minNPoints=4, nPoints=5, ...){
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len = length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    upper <- zoneB["1"]
    lower <- zoneB["-1"]
    xZoned <- ifelse(x > upper, 1, ifelse(x < lower, -1, 0))
    violation <- rep(0, times = len)
    if(all(xZoned[1:minNPoints]==1)|all(xZoned[1:minNPoints]==-1)){violation[minNPoints]=1}
    for(i in nPoints:len ){
        temp <- xZoned[(i-nPoints+1):i]
        violation[i] = ifelse(((sum(temp == 1) >= minNPoints) & temp[length(temp)] == 1) | ((sum(temp == -1) >= minNPoints) & temp[length(temp)] == -1), 1, 0)
    }
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}

#' Rule 7
#'
#' Fifteen points in a row are all within 1 standard deviation of the mean on either side of the mean.
#' @param x Numerical vector
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param zoneB Vector of zone borders
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' equality used during evaluation
#' @examples
#' limits = CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
#' zones = CalculateZoneBorders(limits)
#' Rule7(x = rnorm(20), zoneB = zones, nPoints = 15)
#' #zones is object created by function CalculateZoneBorders()
#' @export
Rule7 <- function(x, nPoints=15, zoneB, ...){
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len = length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    upper <- zoneB["1"]
    lower <- zoneB["-1"]
    xZoned <- (x <= upper & x >= lower)
    violation <- rep(0, times = len)
    for(i in nPoints:len ){
        temp <- xZoned[(i - nPoints+1):i]
        violation[i] = all(temp)
    }
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}

#' Rule 8
#'
#' Eight points in a row outside 1 standard deviation of the mean in both directions.
#' @param x Numerical vector
#' @param nPoints Sequence of consequtive points to be evaluated
#' @param zoneB Vector of zone borders
#' @param ... unspecified arguments of a function
#' @return Vector of the same length as x
#' @details
#' 0 means: ok \cr
#' 1 means: violation \cr
#'
#' inequality used during evaluation
#' During calculation of EvaluateRules function wiht controlLimitDistance <= 1, the evaluation of this rule is suppressed
#' @examples
#' limits = CalculateLimits(x = rnorm(10), lcl = NA, cl = 100, ucl = NA, type = 'i')
#' zones = CalculateZoneBorders(limits)
#' Rule8(x = rnorm(20), zoneB = zones, nPoints = 8)
#' #zones is object created by function CalculateZoneBorders()
#' @export
Rule8 <- function(x, nPoints=8, zoneB, ...){
    xOriginal = x
    x = na.omit(x)
    lenorig = length(xOriginal)
    len = length(x)
    if(len < nPoints){
        return(rep(0, times = lenorig))
    }
    upper <- zoneB["1"]
    lower <- zoneB["-1"]
    xZoned <- (x > upper | x < lower)
    violation <- rep(0, times = len)
    for(i in nPoints:len){
        temp <- xZoned[(i - nPoints+1):i]
        violation[i] = all(temp)
    }
    violation <- replace(xOriginal, !is.na(xOriginal), violation)
    violation[is.na(violation)] <- 0
    return(violation)
}
