#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# author__ = "SPSS, JKP"
# version__ = "1.0.1"

# History
# 14-apr-2014 Original Version


helptext='Disaggregate (distribute) a time series to a higher
frequency, optionally with the use of other disaggregated correlated series

STATS DISAGG LOWFREQ=series HIGHFREQ=list of series
    LOWFREQSTART=datespec HIGHFREQSTART=datespec FREQUENCY=integer
    USEHIGHFREQ=YES* or NO FIRSTPASS=YES or NO*
/OPTIONS CONVERSIONTYPE=SUM* or AVERAGE or FIRST or LAST
    CONVERSIONMETHOD= CHOWLINMAXLOG* or CHOWLINMINRSSECO or CHOWLINMINRSSQUI
    or CHOWLINFIXED or FERNANDEZ or LITTERMANMAXLOG
    or LITTERMANMINRSS or LITTERMANFIXED or DENTONCHOLETTE
    or DENTON or OLS
    MINRHO=number DIFFDEGREE = 0* or 1 or 2
    DENTONCRIT = PROPORTIONAL* or ADDITIVE
/OUTPUT OUTPUTDS=dataset name PLOT=YES* or NO.

For example,
STATS DISAGG LOWFREQ=annualSales HIGHFREQ=quarterlyGNP
FREQUENCY=4 LOWFREQSTART = 2000 HIGHFREQSTART=2000 1
/OUTPUT OUTPUTDS=quarterlySales.
   
STATS GET R /HELP.  prints this information and does nothing else.
Default values are marked with *
Whether or not a keyword is required is indicated in the description.

This command must be executed twice in order to obtain the disaggregated series
unless there are no high frequency indicator variables.  (This is necessary
because high and low frequency series cannot exist in the same dataset.)
The first time the low frequency dataset must be active, and the LOWFREQ keyword
must be supplied.  The command is followed by DATASET ACTIVE to make the
high frequency series dataset active, and STATS DISAGG is executed again
to fetch these variables.  The disaggregated series is written to a new dataset.

All keywords can be specified on either pass except that LOWFREQ must
be given on the first pass.

LOWFREQ (required) specifies the series to be disaggregated.

HIGHFREQ (optional) specifies one or more indicator series to
be used in disaggregating the low frequency series.  If HIGHFREQ
is not used, the disaggregated series will be constant within the
time points for each particular low frequency time point.

LOWFREQSTART and HIGHFREQSTART (required) specify the starting
time of the low frequency series and, if HIGHFREQ is used, the
high frequency series.  The value can be a single number such
as a year, or it can be two numbers such a year and month.
The second value is assumed to be 1 if omitted.
If the data are not time series, use starting values of 1.

FREQUENCY specifies the frequency ratio of the low frequency
series and the disaggregated series.  For example, to disaggregate
quarterly data to monthly, the value would be 3.  The value must
be an integer.

USEHIGHFREQ = NO can be specified if there will be no
high frequency indicators.  If specified, the disaggregation
occurs on a single pass.

This command expects to take two passes (unless USEHIGHFREQ=NO).
Therefore, it saves information from the first pass and expects
a second pass to follow.  If the first pass is run but the
procedure needs to start over with a new first pass, specify
FIRSTPASS=YES to cause the saved information to be discarded.

The OPTIONS subcommand specifies how the disaggregation is
to be done.

CONVERSIONTYPE (optional) specifies the relationship of the 
disaggregated values to the original.  SUM and AVERAGE specify
that the sum or average of the entire disaggregated series equals
the corresponding statistic for the low frequency series.
FIRST and LAST specify that the first or last value of
the disaggregated series equals the corresponding value
of the low frequency series.

CONVERSIONMETHOD (optional) specifies the conversion algorithm.  The
Chow-Lin, Litterman, and Fernandez methods (with variations)
use generalized least squares of the low frequency variable
on the aggregated values of the high frequency series and
then distribute the differences in the result across the
quarterly values.  The estimated relationship between the
low frequency series (actual and aggregated) is assumed to
hold also for the high frequency series.  The GLS estimators
include estimation of an autoregression parameter, rho. 
The OLS method does ordinary least squares (with no
autocorrelation parameter).  These methods all require at 
least one high frequency indicator.

The remaining methods (Denton type) can be used with or without
an indicator variable and  minimize the sum of squared
deviations of the low and high frequency series using
two additional parameters.  Only one indicator variable
is allowed.

For details on these algorithms, see
http://journal.r-project.org/archive/2013-2/sax-steiner.pdf

MINRHO (optional) specifies the minimum value of the
autocorrelation parameter and defaults to 0. Applies to
GLS methods only.

DIFFDEGREE (optional) can be 0, 1, or 2 to determine whether
Denton methods use levels, first difference, or 
second difference in the minimization.

DENTONCRIT (optional) specifies whether Denton methods use
proportional or absolute deviations in the minimization.

The OUTPUT subcommand specifies the desired output.

OUTPUTDS (required) specifies a name for the output
dataset.  The dataset will contain the disaggregated series
and a time variable.  The dataset name must not already be
in use.  Period 1 in the time variable has value 0
even though the starting values count from 1.  For example,
a starting value of 2000 1 produces a first time value of
2000.00.

PLOT specifies whether or not to plot the actual and fitted
low frequency values and residuals.

'


gtxt <- function(...) {
    return(gettext(...,domain="STATS_DISAGG"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_DISAGG"))
}

# keyword-mapping lookup for method
methods = list(chowlinmaxlog="chow-lin-maxlog", chowlinminrsseco="chow-lin-minrss-ecotrim", 
    chowlinminrssqui="chow-lin-minrss-quilis", chowlinfixed="chow-lin-fixed", 
    fernandez="fernandez", littermanmaxlog="litterman-maxlog", 
    littermanminrss="litterman-minrss", littermanfixed="litterman-fixed", 
    dentoncholette="denton-cholette", denton="denton", uniform="uniform", ols="ols")

# parameters to passed to td
keeplist = c("formula", "to", "conversion", "method", "truncated.rho",
        "h", "criterion")

# main routine
dodisagg = function(lowfreq=NULL, highfreqds=NULL, usehighfreq=TRUE, highfreq=NULL, 
            lowfreqstart=NULL, highfreqstart=NULL, to=NULL,
            conversion=NULL, method=NULL,
            truncated.rho=NULL, h=NULL, criterion=NULL,
            outputds=NULL, plotit=NULL, firstpass=FALSE) {
    
    setuplocalization("STATS_DISAGG")
    
    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output
    procname=gtxt("Disaggregate Time Series")
    warningsprocname = gtxt("Disaggregate Series: Warnings")
    omsid="STATSDISAGG"
    warns = Warn(procname=warningsprocname,omsid=omsid)
    tryCatch(library(tempdisagg), error=function(e){
        warns$warn(gtxtf(
            "The R %s package is required but could not be loaded.", "tempdisagg"),
            dostop=TRUE)
    }
    )
    
    # This procedure requires two passes if there are any
    # higher frequency variables.  The first pass retrieves the
    # low frequency series that is to be distributed.  In-between,
    # the active dataset is switched to the higher frequency dataset,
    # and the second pass gets that data and carries out the distribution.
    # If there are no higher frequency variables, usehighfreq should be
    # false.
    # The high frequency specs can be supplied on either pass, except
    # as necessary on a one-pass usage. 
    # The two-pass structure means that some of the automatic
    # parameter validation mechanisms can't be used, because some
    # required arguments can be supplied on either pass
    # FIRSTPASS = YES can be used to cancel the effect of a first call
    # where the second call was not made

    if (firstpass) {
        invisible(tryCatch(rm(allargs, envir=.GlobalEnv), 
            error=function(e) {}, warning=function(e) {}))
    }
    if (!exists("allargs")) {
        firstrep = TRUE
        if (is.null(lowfreq)) {
            warns$warn(gtxt(
                "STATS DISAGG must be called with the low frequency series before calling with any high frequency series."),
                dostop=TRUE)
        } else {
            lowfreqseries = tryCatch(
                spssdata.GetDataFromSPSS(lowfreq, missingValueToNA=TRUE),
                error=function(e) {
                    warns$warn(e$message, dostop=TRUE)
                })
            allargs = as.list(environment())
            allargs["lowfreqseries"] = NULL
            assign("allargs", allargs, .GlobalEnv)
            assign("lowfreqseries", lowfreqseries, .GlobalEnv)
            # end of first pass
            warns$warn(gtxt("Low frequency step completed"), dostop=FALSE)
            if (usehighfreq) {
                return()
            }
        }
    } 
    # second  or only pass

    firstrep = FALSE
    ne = new.env()  # where to keep formula and data series
    # combine specs from both passes (a no-op if only one pass)
    # only function call parameters from pass2 are merged
    contents = as.list(environment())
    contents = contents[names(contents) %in% names(formals())]
    allargs = merged(allargs, contents)

    # fill in defaults if not specified on either pass
    if (is.null(allargs[["conversion"]])) {
        allargs[["conversion"]] = "sum"
    }
    if (is.null(allargs[["method"]])) {
        allargs[["method"]] = "chowlinmaxlog"
    }
    if (is.null(allargs[["criterion"]])) {
        allargs[["criterion"]] = "proportional"
    }
    if (is.null(allargs[["truncated.rho"]])) {
        allargs[["truncated.rho"]] = 0
    }
    if (is.null(allargs[["h"]])) {
        allargs[["h"]] = 0
    }
    if (is.null(allargs[["plotit"]])) {
        allargs[["plotit"]] = TRUE
    }
    if (is.null(allargs[["outputds"]])) {
        rm(allargs, envir=.GlobalEnv)
        warns$warn(gtxt("An output dataset name is required but was not specified"),
                   dostop=TRUE)
    }
    if (allargs[["outputds"]] %in% spssdata.GetDataSetList()) {
        rm(allargs, envir=.GlobalEnv)
        warns$warn(gtxt("The output dataset name is already in use."), dostop=TRUE)
    }
    if (any(is.null(allargs[["lowfreqstart"]]), 
        (!is.null(allargs[["highfreq"]]) && is.null(allargs[["highfreqstart"]])) 
    )) {
            rm(allargs, envir=.GlobalEnv)
            warns$warn(gtxt("Starting time must be specified for all series"), dostop=TRUE)
    }
    if (is.null(allargs[["to"]])) {
        rm(allargs, envir=.GlobalEnv)
        warns$warn(gtxt("Target frequency is required but was not specified"), dostop=TRUE)
    }
    # get the rhs variables and convert to time series
    if (!is.null(allargs[["highfreq"]])) {
        if (length(allargs[["highfreq"]]) > 1 && 
           allargs[["method"]] %in% c("dentoncholette", "denton", "uniform")) {
            allargs[["method"]] = "chowlinmaxlog"
            warns$warn(gtxt("Disaggregation method set to Chowlinmaxlog, because Denton methods allow only one indicator"),
               dostop=FALSE)
            
        }
        dta = tryCatch(
            spssdata.GetDataFromSPSS(allargs[["highfreq"]], missingValueToNA=TRUE),
            error=function(e) {return(e$message)}
        )
        if (!class(dta) == "data.frame") {
            rm(allargs, envir=.GlobalEnv)
            warns$warn(dta, dostop=TRUE)
        }
        highlen = length(allargs[["highfreq"]])
        for (v in allargs[["highfreq"]]) {
            assign(v, 
                ts(dta[v],start=allargs[["highfreqstart"]],
                frequency=allargs[["to"]]), envir=ne)
        }
    }
    else {
        # no high freq variables
        if (is.null(method) ||!(method %in% c("dentoncholette", "denton", "uniform"))) {
            allargs[["method"]] = "dentoncholette"
            warns$warn(gtxt("Disaggregation method set to Denton-Cholette, because no high frequency variables were specified"),
                dostop=FALSE)
        }
    }
    allargs[["method"]] = methods[[allargs[["method"]]]]
    assign(allargs[["lowfreq"]], ts(lowfreqseries, start=allargs[["lowfreqstart"]]), envir=ne)

    if (is.null(allargs[["highfreq"]])) {
        allargs[["formula"]] = as.formula(buildformula(allargs[["lowfreq"]], NULL),
           env=ne)
    } else {
        allargs[["formula"]] = as.formula(buildformula(allargs[["lowfreq"]],
            allargs[["highfreq"]]),
           env=ne)
    }

    tdallargs = allargs[names(allargs) %in% keeplist]
    res = tryCatch(do.call(td, tdallargs),
       error = function(e) {
           return(e$message)
       }
    )
    if (class(res) != "td") {
        rm(allargs, envir=.GlobalEnv)
        warns$warn(res, dostop=TRUE)
    }

    displayresult(allargs, res, warns)
    makedataset(allargs, res, warns)
    invisible(tryCatch(rm(allargs, envir=.GlobalEnv), warnings=function(e) {return(NULL)}))
}

merged = function(lis1, lis2) {
    # return list containing all non-null items in the union of the arguments
    # lis1 and lis2 are lists with all items named
    # lis2 takes priority
    
    res = lis1[!sapply(lis1, is.null)]
    for (item in names(lis2)) {
        if (!is.null(lis2[[item]])) {
            res[[item]] = lis2[[item]]
        }
    }
    return(res)
}

buildformula = function(lowfreq, highfreq) {
    # return formula as needed by td function
    # lowfreq is the name of the low frequency series
    # highfreq is a possibly null list of high freq series
    
    if (is.null(highfreq)) {
        highfreq = "1"
    } else {
        highfreq = paste(highfreq, collapse="+")
    }
    return(paste(lowfreq, "~", highfreq, sep="" ))
}

conversiontrans = list(sum=gtxt("sum"), average=gtxt("average"), first=gtxt("first"), last=gtxt("last"))
dentoncrittrans = list(proportional=gtxt("proportional"), additve=gtxt("additive"))
displayresult = function(allargs, res, warns) {
    # Display result, if any
    # Produce pivot tables and charts
    
    StartProcedure(allargs[["procname"]], allargs[["omsid"]])
    summarylabels=list(
        gtxt("Low-Frequency Variable"),
        gtxt("Low-Frequency Start"),
        gtxt("High-Frequency Variables"),
        gtxt("High-Frequency Start"),
        gtxt("High-Frequency Dataset"),
        gtxt("Target Frequency"),
        gtxt("Disaggregation Method"),
        gtxt("Conversion Type"),
        gtxt("Denton Criterion"),
        gtxt("Denton Differencing"),
        gtxt("Output Dataset"),
        gtxt("R Squared"),
        gtxt("Adjusted R Squared"),
        gtxt("Log Likelihood"),
        gtxt("Degrees of Freedom"),
        gtxt("AIC"),
        gtxt("BIC"),
        gtxt("Rho"),
        gtxt("Rho Lower Bound")
    )

    summaryvalues = list(
        allargs[["lowfreq"]],
        paste(allargs[["lowfreqstart"]], sep=":"),
        paste(allargs[["highfreq"]], collapse=", "),
        ifelse(length(allargs[["highfreqstart"]]) > 0, 
            paste(allargs[["highfreqstart"]], collapse=":"), gtxt("NA")),
        dhelp(allargs[["highfreqds"]]),
        allargs[["to"]],
        allargs[["method"]],
        conversiontrans[[allargs[["conversion"]]]],
        dentoncrittrans[[allargs[["criterion"]]]],
        dhelp(allargs[["h"]]),
        allargs[["outputds"]],
        dhelp(res$r.squared),
        dhelp(res$adj.r.squared),
        dhelp(res$logl),
        dhelp(res$df),
        dhelp(res$aic),
        dhelp(res$bic),
        dhelp(res$rho),
        dhelp(allargs[["truncated.rho"]])
    )
    names(summaryvalues) = summarylabels
    summarydf = data.frame(cbind(summaryvalues))
    colnames(summarydf) = gtxt("Values")
    
    spsspivottable.Display(summarydf, title=gtxt("Series Disaggregation Summary"), 
                           templateName="STATSDISAGGSUMMARY",
                           caption=gtxt("Results computed by R tempdisagg package"),
                           isSplit=FALSE
    )
    if (!is.null(allargs[["highfreq"]])) {
        coefs = data.frame(summary(res)$coefficients)
        names(coefs) = c(gtxt("Estimate"), gtxt("Std. Error"),
            gtxt("t"), gtxt("Sig."))
        spsspivottable.Display(coefs, title=gtxt("Disaggregation Indicator Coefficients"),
            templateName="STATSDISAGGCOEFS",
            isSplit=FALSE
        )
    }
    if (allargs[["plotit"]]) {
        plot(res)
    }
    #spsspkg.EndProcedure()
    warns$display(inproc=TRUE)
}

dhelp = function(value, decimals=4) {
    # return translated, rounded value for display
    if (is.null(value) || is.na(value)) {
        return(gtxt("NA"))
    } else if (is.numeric(value)) {
            return(round(value, decimals))
    } else {
        return(value)
    }
}

makedataset = function(allargs, res, warns) {
    # create output dataset for series
    # specs are known to be valid

    df = data.frame(time=time(res$values), values=res$values)
    dict = spssdictionary.CreateSPSSDictionary(
        c("Time", gtxt("Time"), 0, "F8.2", "scale"),
        c("Series", 
        gtxtf("Distributed from %s via %s", allargs[["lowfreq"]], allargs[["method"]]),
            0, "F8.2", "scale")
    )
    spssdictionary.SetDictionaryToSPSS(allargs[["outputds"]], dict)
    spssdata.SetDataToSPSS(allargs[["outputds"]], df)
    spssdictionary.EndDataStep()
}


Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                    },
                    error = function(e) {
                        FALSE
                    }
                )
            } else {
                procok = TRUE
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                    gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                        spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}


# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 
# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}



Run = function(args) {
    #Execute the STATS DISAGG command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("LOWFREQ", subc="",  ktype="varname", var="lowfreq"),
        spsspkg.Template("USEHIGHFREQ", subc="", ktype="bool", var="usehighfreq"),
        spsspkg.Template("HIGHFREQ", subc="", ktype="varname", var="highfreq", islist=TRUE),
        spsspkg.Template("HIGHFREQDS", subc="", ktype="varname", var="highfreqds"),
        spsspkg.Template("LOWFREQSTART", subc="", ktype="int", var="lowfreqstart", islist=TRUE),
        spsspkg.Template("HIGHFREQSTART", subc="", ktype="int", var="highfreqstart", islist=TRUE),
        spsspkg.Template("FREQUENCY", subc="", ktype="int", var="to", vallist=list(1)),
        spsspkg.Template("FIRSTPASS", subc="", ktype="bool", var="firstpass"),
        
        spsspkg.Template("CONVERSIONTYPE", subc="OPTIONS", ktype="str", var="conversion",
            vallist=list("sum", "average", "first", "last")),
        spsspkg.Template("CONVERSIONMETHOD", subc="OPTIONS", ktype="str", var="method",
            vallist=list("chowlinmaxlog", "chowlinminrsseco", "chowlinminrssqui",
                "chowlinfixed", "fernandez", "littermanmaxlog", "lit
                
                termanminrss",
                "littermanfixed", "dentoncholette", "denton", "uniform", "ols")),
        spsspkg.Template("MINRHO", subc="OPTIONS", ktype="float", var="truncated.rho",
            vallist=list(-1, 1)),
        spsspkg.Template("DENTONDIFF", subc="OPTIONS", ktype="int", var="h",
            vallist=list(0, 2)),
        spsspkg.Template("DENTONCRIT", subc="OPTIONS", ktype="str", var="criterion",
            vallist=list("proportional", "additive")),
        
        spsspkg.Template("OUTPUTDS", subc="OUTPUT", ktype="varname", var="outputds"),
        spsspkg.Template("PLOT", subc="OUTPUT",  ktype="bool", var="plotit")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "dodisagg")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
