#/***********************************************************************
# * (C) Copyright Jon K Peck, 2026
# ************************************************************************/

# version 1.0.1

# history
# feb 2026    Initial version
# apr-8-2026  suppress plot failure when no discrimination



# helpers
gtxt <- function(...) {
    return(gettext(...,domain="STATS_MIXED_CLUSTER2"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_MIXED_CLUSTER2"))
}

loadmsg = "The R %s package is required but could not be loaded."
tryCatch(suppressWarnings(suppressPackageStartupMessages(library(VarSelLCM, warn.conflicts=FALSE))), error=function(e){
    stop(gtxtf(loadmsg,"varSelLCM"), call.=FALSE)
}
)

tryCatch(suppressWarnings(suppressPackageStartupMessages(library(R.utils, warn.conflicts=FALSE))), error=function(e){
    stop(gtxtf(loadmsg,"R.utils"), call.=FALSE)
}
)

# Override for function in VarSelClus
proba.post <- function(object, newdata){
    ###print("in replacement") #dbg
    logprob <- matrix(object@param@pi, nrow(newdata), object@model@g, byrow=TRUE)
    for (nom in colnames(newdata)){
        xnotna <- newdata[,which(colnames(newdata)==nom)]
        where <- which(!is.na(xnotna))
        xnotna <- xnotna[where]
        if (nom %in% rownames(object@param@paramContinuous@mu)){
            who <- which(nom == rownames(object@param@paramContinuous@mu))
            for (k in 1:object@model@g) logprob[where,k] <- logprob[where,k] + dnorm(xnotna, object@param@paramContinuous@mu[who,k], object@param@paramContinuous@sd[who,k], log=TRUE)
        }else if (nom %in% rownames(object@param@paramInteger@lambda)){
            who <- which(nom == rownames(object@param@paramInteger@lambda))
            for (k in 1:object@model@g) logprob[where,k] <- logprob[where,k] + dpois(xnotna, object@param@paramInteger@lambda[who,k], log=TRUE)
        }else if (nom %in% names(object@param@paramCategorical@alpha)) { ###
            who <- which(nom ==  names(object@param@paramCategorical@alpha))
            for (k in 1:object@model@g){
                for (h in 1:ncol(object@param@paramCategorical@alpha[[who]]))
                    logprob[where,k] <- logprob[where,k] + log(object@param@paramCategorical@alpha[[who]][k,h] ** (xnotna == colnames(object@param@paramCategorical@alpha[[who]])[h]))
            } ###
        }
    }
    prob <- exp(logprob - apply(logprob, 1, max))
    prob/rowSums(prob)
}

mylist2env = function(alist) {
    env = new.env()
    lnames = names(alist)
    for (i in 1:length(alist)) {
        assign(lnames[[i]],value = alist[[i]], envir=env)
    }
    return(env)
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
    lcl = mylist2env(lcl) # makes this list into an environment
    
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
            spssdata.CloseDataConnection()
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
                procok = TRUE
            }
        } else {
            procok = inproc
            if (!inproc) {
                procok =tryCatch({
                    spsspkg.StartProcedure(lcl$procname, lcl$omsid)
                    procok = TRUE
                },
                error = function(e) {
                    prockok = FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings and Messages","Warnings", isSplit=FALSE) # do not translate this
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


casecorrect = function(vlist, vardict, warns) {
    # correct the case of variable names
    # vlist is a list of names, possibly including TO and ALL
    # vardict is a variable dictionary
    # unrecognized names are returned as is as the GetDataFromSPSS api will handle them
    
    if (length(vlist) == 0) {
        return(vlist)
    }
    dictnames = vardict["varName",]
    names(dictnames) = tolower(dictnames)
    dictnames['all'] = "all"
    dictnames['to'] = "to"
    correctednames = list()
    for (item in vlist) {
        lcitem = tolower(item)
        itemc = dictnames[[lcitem]]
        if (is.null(itemc)) {
            warns$warn(gtxtf("Invalid variable name: %s", item), dostop=TRUE)
        }
        correctednames = append(correctednames, itemc)
    }
    return(correctednames)
}

failifexist = function(vlist, vardict, warns) {
    # check whether any names in vlist appear in vardict and stop if so
    
    dictnames = vardict["varName",]
    names(dictnames) = tolower(dictnames)
    for (item in vlist) {
        if (item %in% dictnames) {
            warns$warn(gtxtf("Output variable already exists: %s", item), dostop=TRUE)
        }
    }
}


procname=gtxt("Mixed Cluster2")
warningsprocname = gtxt("Mixed Cluster2 Messages and Warnings")
omsid="STATSMIXEDCLUSTER2"

# main worker
domixed2 = function(variables=NULL, integervars=NULL, ncomponents=NULL,
            selectvars=TRUE, selcriterion="BIC", usemodelfile=NULL, plots=TRUE,
            outmodel=NULL, clustervar, clusterprobsroot=NULL, nbcores=1,
            timelimit=Inf) 
    {

    # DEBUG
     # sink(file="c:/temp/mixed2.log", type="output")
     # f = file("c:/temp/mixed2msgs.log", open="w")
     # sink(file=f, type="message")
       domain<-"STATS_MIXED_CLUSTER2"
    setuplocalization(domain)
     
     # varselCluster does not work correctly with multiple cores
     # The discrim result is incomplete, and, hence, the plot fails
     # Leaving nbcomes in the syntax definition for, perhaps, the future..
     nbcores = 1
     warns = Warn(procname=warningsprocname,omsid=omsid)
    usingmodel = !is.null(usemodelfile)
    
    if (is.null(ncomponents) && !usingmodel) {
        warns$warn(gtxt("Number of components must be specified if not using existing model"),
            dostop=TRUE)
    }
    #     ))
    # }
    dslist = spssdata.GetOpenedDataSetList()
    nsplitvars = length(spssdata.GetSplitVariableNames())
    if (nsplitvars > 0) {
        warns$warn(gtxt("Split files is not supported by this procedure"), dostop=TRUE)
    }
    if (!is.null(spssdictionary.GetWeightVariable())) {
        warns$warn(gtxt("Case weights are not supported by this procedure and will be ignored"), dostop=FALSE)
    }
    
    if (usingmodel && !is.null(outmodel)) {
        outmodel = NULL
        warns$warn(gtxt("Cannot save model if using existing model.  Outmodel will be ignored"),
            dostop=FALSE)
    }
    if (usingmodel) {
        tryCatch(
            {#TODO check for var specs
                
            # avoid overwriting output variables
            predclus = clustervar
            predprobs = clusterprobsroot
            load(usemodelfile)
            clustervar = predclus
            clusterprobsroot = predprobs

            if (!exists("varselres")) {
                warns$warn(gtxt("Model file does not contain a model"), dostop=TRUE)
            }
            except = function(e) {
                print(e)
                warns$warn(gtxtf("Model file %s not found or could not be read", usemodelfile),
                    dostop=TRUE)
                }
            }
        )
    }
    if (usingmodel && !exists("varselres")) {
        warns$warn(gtxt("Model file does not contain a model"), dostop=TRUE)
    }
    if (is.null(c(variables, integervars))) {
        warns$warwn(gtxt("No variables were specified"))
    }
    spssdict = spssdictionary.GetDictionaryFromSPSS()
    if (any(sapply(list(ncomponents, clustervar), is.null))  && !usingmodel) {
            warns$warn(gtxt("A required input was not specified"), dostop=TRUE)
    }
    failifexist(c(clustervar), spssdict, warns)

    if (!usingmodel) {
        variables = casecorrect(c(variables), spssdict, warns)
        integervars = casecorrect(c(integervars), spssdict, warns)
    }
    starttime = as.integer(Sys.time())
    # tempcaseid will be used to synchronize new vars with cases in case there is a select in effect
    # It will be deleted at the end unless error
    
    tempcaseid = paste("T", as.character(runif(1,.05,1)), sep="")
    spsspkg.Submit(sprintf("COMPUTE %s = $CASENUM.", tempcaseid))
    spsspkg.StartProcedure(gtxt("Mixed Cluster2"),"STATS MIXED CLUSTER2")
    # GetData will fail if row labels are not unique

    tryCatch(
        {
        dta = spssdata.GetDataFromSPSS(c(variables, integervars), missingValueToNA=TRUE, factorMode="levels",
            keepUserMissing=FALSE, row.label=tempcaseid)
        },
        error=function(e) {
            print(e)
            warns$warn(gtxtf("The required variables are %s", 
                paste(c(variables, integervars), collapse=" ", sep=" ")))
            
            warns$warn(paste(gtxtf("error fetching data\n%s", e), sep="\n"), dostop=TRUE)
        }
    )

    # procedure handles missing data
    if (length(integervars) > 0) {
        dta = integerize(dta, integervars, warns)
    }
    if (!usingmodel) 
        {
        selcriterion = toupper(selcriterion)
        if ((selectvars && !(selcriterion %in% c("AIC", "BIC", "MICL"))) ||
            (!selectvars && !(selcriterion %in% list("AIC", "BIC", "ICL")))) {
            warns$warn(gtxtf("Invalid variable selection criterion: %s", selcriterion), dostop=TRUE)
        }
        tryCatch(withTimeout(
            tryCatch(
                {
                invisible(tryCatch(rm("varselres"), warning=function(w) {}))
                varselres =
                    VarSelCluster(x=dta, gvals=ncomponents, vbleSelec = selectvars,
                        crit.varsel=selcriterion)
                estdate <<- date()
                }, error = function(ee) {
                    warns$warn(gtxtf("Estimation Failed: %s", ee), dostop=TRUE)
                }, warning = function(w) {
                    warns$warn(gtxtf("Estimation Warning: %s", w), dostop=TRUE)
                }
            )
        , timeout=timelimit), error = function(e) {
            # message cannot be translated
            warns$warn(sprintf("Procedure stopped.  Time limit of %s seconds has expired",
                timelimit))}
        )
        if (!exists("varselres")) {
           warns$warn(gtxt("Procedure is unable to find any clusters"), dostop=TRUE)
        }
    }

    if (usingmodel) {
        # substituting corrected Predict function
        resproba = proba.post(varselres, newdata=dta)
    }
    if (!is.null(outmodel)) {
        estdate = date()
        save(varselres, estdate, variables, integervars, clustervar, clusterprobsroot, file=outmodel)
        warns$warn(gtxtf("Estimated model saved to file %s", outmodel), dostop=FALSE)
    }
 
    caption = gtxtf("package VarSelLCM, version %s", packageVersion("VarSelLCM"))
    if (!usingmodel) {
        displayresults(varselres, dta, usemodelfile, outmodel, selectvars, selcriterion, 
            ncomponents, nbcores, caption, clustervar, clusterprobsroot, estdate, starttime, warns)
        if (plots) {
            # plots often produce useless "discouraged" warnings, so they are suppressed
            failmsg = gtxt("Unable to produce the plot.  Model is deficient or discrimination is zero")
            if (all(varselres@criteria@discrim == 0)) {
                warns$warn(failmsg, dostop=FALSE)
            } else {
                tryCatch(
                    suppressWarnings(plot(varselres)),
                error = function(e) {warns$warn(failmsg,
                    dostop=FALSE)}
            )
            }
        }
    }
    spsspkg.EndProcedure()

    if (!usingmodel) {
        writedataset(varselres, usemodelfile, clustervar, clusterprobsroot, dta, outmodel, spssdict, tempcaseid, estdate, warns)
    } else {
        warns$warn(gtxtf("Output variables or root: %s: ", 
            paste(clustervar, clusterprobsroot, sep=" ", collapse=" ")))
        writedataset(resproba, usemodelfile, clustervar, clusterprobsroot, dta, outmodel, spssdict, tempcaseid, estdate, warns)
    }
        warns$display(inproc=FALSE)
    # DEBUG
    # sink(file=NULL, type="output")
    # sink(file=NULL, type="message")

}



integerize = function(dta, integervars, warns) {
    # convert integer input variables to type integer

    # dta is the data frame with integervars, if any, at the end
    # integervars is the list of integer variable names

    if (length(integervars) == 0) {
        return(dta)
    }
    problemvars = list()
    interror = NULL
    for (v in integervars) {
        if (is.factor(dta[[v]])) {
            warns$warn(gtxtf("Integer variables must have a scale measurement level: %s", v), dostop=TRUE)
        }

        tryCatch({dta[v]=as.integer(dta[[v]])}, warning=function(w) {interror<<-v})
        problemvars = append(problemvars, interror)
        interror = NULL
    }

    if (length(problemvars) > 0) {
        plist = paste(problemvars, collapse=", ")
            warns$warn(gtxtf("A nonnumeric or negative value was found in an integer variable.  Set to missing:\n %s",
                plist), dostop=FALSE)
    }
    
    return(dta)
}


writedataset = function(res, usemodelfile, clustervar, clusterprobsroot, dta, outmodel, 
    spssdict, idvar, estdate, warns) {
    # add cluster variables to active file
    
    # res is the cluster information if usemodelfile is FALSE and the
    # predicted probabilities if TRUE
    # dta is the new or old data for predict or fitted
    # clustervar is the name for the estimated cluster
    # clusterprobs is the root name for the probability variables or NULL
    # spssdict is a dictionary for the active dataset
    # idvar is the name for a variable containing the case ids to match on
    # estdate is the date the model was estimated
    
    # dictionary structure for id and cluster number
    
    if (is.null(dta)) {
        warns(gtxt("Estimation or new data dataset is required"), dostop=TRUE)
    }

    iddict = c(idvar, "", 0, "f8.0", "nominal")
    dictlist = spssdictionary.CreateSPSSDictionary(iddict)
    vnames = tolower(spssdict['varName', ])
    outdata= data.frame(row.names(dta))
    if (is.null(usemodelfile)) { # using estimation data
        # update active file from estimates
        outdata = cbind(outdata, data.frame(fitted(res, type="partition")))

        if (!is.null(clusterprobsroot)) {
            outdata = cbind(outdata, data.frame(fitted(res, type="probability")))
        }
    } else { # using probabilities for new data
        outdata = cbind(outdata, data.frame(apply(res, 1, which.max)))
        if (!is.null(clusterprobsroot)) {
            outdata = cbind(outdata, res)
        }
    }
    colnames(outdata)[[1]] = idvar
    colnames(outdata)[[2]] = clustervar
    if (!is.null(clusterprobsroot)) {
        nprob = ncol(outdata) - 2
        for (v in 1:nprob) {
            colnames(outdata)[[v+2]] = paste(clusterprobsroot, "_", v, sep="")
        }
    }
    # make an SPSS dictionary for the output

    # cluster variable
    varspec = list()
    # cluster var name 
    varspec[[1]] = clustervar
 
    # cluster number
    varspec[[2]] = "Cluster"
    varspec[[3]] = 0
    varspec[[4]] = "F8.0"
    varspec[[5]] = "nominal"
    dictlist[[2]] =  unlist(varspec)
    
    # cluster probabilities
    if (!is.null(clusterprobsroot)) {
        nprob = ncol(outdata) - 2
        for (v in 1:nprob) {
            varspec[[1]] = paste(clusterprobsroot, "_", v, sep="")
            if (tolower(varspec[[1]]) %in% vnames) {
                warns$warn(gtxtf("Probability variable names cannot already be in use: %s", varspec[[1]]),
                    dostop=TRUE)
            }
            varspec[[2]] = sprintf("Class %s Probability", v)
            varspec[[3]] = 0
            varspec[[4]] = "F8.3"
            varspec[[5]] = "scale"
            dictlist[[v+2]] =  unlist(varspec)
        }
    }
    dict = spssdictionary.CreateSPSSDictionary(dictlist)
    csvtospss(dict, outdata, idvar)
    warns$warn(gtxtf("Predictions saved to %s", paste(colnames(outdata)[2:ncol(outdata)], collapse=" ")))
}


displayresults = function(res, dta, usemodelfile, outmodel, 
    vbleSelec, crit.varsel, ncomponents, nbcores,
    caption, clustervar, clusterprobs, estdate, starttime, warns)
    {
    elapsedtime = as.integer(Sys.time()) - starttime
    summ = capture.output(summary(res))
    warns$warn(paste(summ[4:length(summ)], collapse="\n"))
    labels = list(
        gtxt("Variables"),
        gtxt("Number of Clusters to Consider"),
        gtxt("Best Number of Clusters"),
        gtxt("Input Model File"),
        gtxt("Output Model File"),
        gtxt("Cluster Variable"),
        gtxt("Cluster Probabilities Root Name"),
        "AIC",
        "BIC",
        "ICL",
        "MICL",
        gtxt("Variable Selection"),
        gtxt("Variable Selection Criterion"),
        gtxt("Relevant Variables"),
        gtxt("Irrelevant Variables"),
        gtxt("Log Likelihood"),
        gtxt("Elapsed Time (Seconds"),
        gtxt("Estimation Date")
    )

    values = list(
        paste(res@data@var.names, collapse=" "),
        paste(ncomponents, collapse=", "),
        res@model@g,
        ifelse(is.null(usemodelfile), "--", usemodelfile),
        ifelse(is.null(outmodel), "--", outmodel),
        clustervar,
        ifelse(is.null(clusterprobs), "--", paste(clusterprobs, collapse=" ")),
        ifelse(res@criteria@AIC == 0, "--", sprintf("%.4f", res@criteria@AIC)),
        ifelse(res@criteria@BIC == 0, "--", sprintf("%.4f", res@criteria@BIC)),
        ifelse(res@criteria@ICL == 0, "--", sprintf("%.4f", res@criteria@ICL)),
        ifelse(res@criteria@MICL == 0, "--", sprintf("%.4f", res@criteria@MICL)),
        ifelse(vbleSelec, gtxt("Yes"), gtxt("No")),
        ifelse(vbleSelec == 0, "--", crit.varsel),
        paste(res@model@names.relevant, collapse=" "),
        paste(res@model@names.irrelevant, collapse=" "),
        res@criteria@loglikelihood,
        elapsedtime,
        estdate
    )

df = cbind(values)
df = data.frame(df, row.names=labels)
spsspivottable.Display(
    df, 
    title=gtxt("Clustering Parameters"),
    outline=gtxt("Clustering Parameters"),
    templateName="STATSVARSELPARMS",
    caption=caption
)
disc = data.frame(res@criteria@discrim)
colnames(disc) = gtxt("Power")
spsspivottable.Display(
    disc,
    rowdim=gtxt("Variable"),
    title=gtxt("Discriminative Power"),
    templateName='STATSVARSELDISCRIM'
)


}
 

csvtospss = function(dict, preds, idvar) {
    # save a temporary csv file and merge into SPSS
    
    # dict is the active file dictionary
    # preds is the data to add
    # idvar is the BY variable for merging
    # idvar is DELETED from the active file during the merge
    
    # due to locale and encoding issues, we can't use a simple Submit
    # to do the Submit, so a temporary file with forced
    # encoding setting and INSERT is used
    
    csvfile = tempfile("csvpred", tmpdir=tempdir(), fileext=".csv")
    write.csv(preds, file=csvfile, row.names=FALSE)
    preddataset = paste("D_", runif(1, .05, 1), collapse="", sep="")
    spsscmd = sprintf('
* Encoding: UTF-8.
        PRESERVE.
        SET DECIMAL DOT.
        GET DATA  /TYPE=TXT
        /FILE="%s"
        /ENCODING="UTF8"
        /DELCASE=LINE
        /DELIMITERS=","
        /QUALIFIER=""""
        /ARRANGEMENT=DELIMITED
        /FIRSTCASE=2
        /VARIABLES=', csvfile)
    
    varspecs = list()
    for (v in 1:ncol(dict)) {
        if (!substr(dict[["varFormat", v]], 1,1) %in% c('A', 'F')) {
            dict[["varFormat", v]] = "F"
        }
    }
    # complete GET DATA command with variable names and formats
    for (v in 1:ncol(dict)) {
        varspecs = append(varspecs, paste(dict[["varName", v]], dict[["varFormat", v]], sep=" "))
    }

    varspecs = paste(varspecs, collapse="\n")
    activedataset = getactivedsname()
    dsn = sprintf("dataset name %s.", preddataset)
    cmd = paste(spsscmd, varspecs, ".\n", dsn, collapse="\n")
    syntemp = tempfile("csvsyn", tmpdir=tempdir(), fileext=".sps")
    writeLines(cmd, con=syntemp, useBytes=TRUE)
    # run the GET DATA command
    spsspkg.Submit(sprintf("INSERT FILE='%s' ENCODING='UTF8'", syntemp))
    spsspkg.Submit("RESTORE.")
    spsspkg.Submit(sprintf("DATASET ACTIVATE %s.", activedataset))
    # UPDATE is used instead of MATCH FILES so that the cluster variable
    # will be from the second file, which matters if the cluster variable exists in the first
    ###if (!is.null(dmerge)) {
    spsspkg.Submit(sprintf("UPDATE /FILE=* /FILE=%s BY %s/DROP=%s.", preddataset, idvar, idvar)) 
    ###spsspkg.Submit(sprintf("UPDATE /FILE=* /FILE=%s BY %s.", preddataset, idvar)) 
    
    ###}
    spsspkg.Submit("EXECUTE")
    unlink(csvfile)
    unlink(syntemp)
    spsspkg.Submit(sprintf("DATASET CLOSE %s", preddataset))
    spsspkg.Submit(sprintf("DATASET ACTIVATE %s", activedataset))
}


getactivedsname = function() {
    # There is no api for this
    
    ds = spssdata.GetOpenedDataSetList()
    spsspkg.Submit("DATASET NAME X44074_60093_")  # renames active dataset
    ds2 = spssdata.GetOpenedDataSetList()
    diff = setdiff(ds, ds2)  # find out which one changed
    spsspkg.Submit("DATASET ACTIVATE X44074_60093_")  # reactivate the previously active one
    cmd = sprintf("DATASET NAME %s", diff)   # and give it back its name
    spsspkg.Submit(cmd)
    return(diff)
}


setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang
    
    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    if (!is.null(fpath)) {
        bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
    }
} 



Run<-function(args){
    cmdname = args[[1]]
    args <- args[[2]]
    
    # variable keywords are typed as varname instead of existingvarlist in
    # order to allow for case correction of names later, since the data fetching apis are
    # case sensitive
    
    oobj <- spsspkg.Syntax(templ=list(
        spsspkg.Template("VARIABLES", subc="", ktype="varname", var="variables", islist=TRUE),
        spsspkg.Template("COUNTVARS", subc="", ktype="varname", var="integervars", islist=TRUE),
        spsspkg.Template("NCOMPONENTS", subc="",ktype="int", var="ncomponents", 
            vallist=list(1, 1000), islist=TRUE),

        spsspkg.Template("USEMODELFILE", subc="", ktype="literal", var="usemodelfile", islist=FALSE),

        spsspkg.Template("CLUSTERVAR", subc="", ktype="varname", var="clustervar", islist=FALSE),
        spsspkg.Template("CLUSTERPROBROOT", subc="", ktype="varname", var="clusterprobsroot", islist=FALSE),
        spsspkg.Template("OUTMODELFILE", subc="", ktype="literal", var="outmodel", islist=FALSE),
        spsspkg.Template("TIMELIMIT", subc="", ktype="int", var="timelimit", islist=FALSE),
        
        spsspkg.Template("PLOT", subc="DISPLAY", ktype="bool", var="plots", islist=FALSE),

        spsspkg.Template("NCORES", subc="OPTIONS", ktype="int", var="nbcores", 
            vallist=list(1, 1000), islist=FALSE),
        spsspkg.Template("TIMELIMIT", subc="OPTIONS", ktype="int", var="timelimit", islist=FALSE),
        spsspkg.Template("SELECTVARS", subc="OPTIONS", ktype="bool", var="selectvars", islist=FALSE),
        spsspkg.Template("SELCRITERION", subc="OPTIONS", ktype="str", var="selcriterion", islist=FALSE,
            vallist=list("aic", "bic", "icl", "micl"))
            ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else {
        res <- spsspkg.processcmd(oobj, args, "domixed2")
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
    
    if (exists("spsspkg.helper")) {
        assign("helper", spsspkg.helper)
    }
}
