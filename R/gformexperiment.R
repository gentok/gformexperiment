utils::globalVariables(c("rows"))

#' Generate experiment start page (HTML)
#'
#' @param survey_json Name of JSON survey file exported by gformexperiment custom add-on. 
#' @param text_randomize Raw full text(s) of the question(s) that are used to randomize experimental conditions. 
#' @param text_starttime Raw full text of the question that is used to record starting time.
#' @param weights_randomize Numeric vector or a list of numeric vectors indicating randomization weights. List length must be equal to the number of randomization questions. Length of each list element must be equal to the number of experimental conditions. 
#' @param timeZone Timezone to record starting time. The default is to set the same time zone as the one set in survey_json.
#' @param file Name and path of the output html file to be saved.
#' 
#' @importFrom jsonlite read_json
#' 
#' @export
genstartpage <- function(survey_json, 
                         text_randomize, 
                         text_starttime=NULL, 
                         weights_randomize = NULL,
                         timeZone = "survey_json timeZone",
                         file=NULL) {
  
  # require(jsonlite)
  
  ## Import survey data
  lco <- read_json(survey_json)

  ## Extract randomization question ids and values
  items_randomize <- lco$items[which(sapply(lco$items,function(x) x$title%in%text_randomize))]
  if (!all(sapply(items_randomize, function(x) as.character(x$type))%in%c("MULTIPLE_CHOICE","LIST"))) stop("text_randomize must correspond to MULTIPLE_CHOICE or LIST items!")
  entry_randomize <- 
    lapply(items_randomize, 
           function(x) as.character(unlist(x$choices)))
  
  items_randomize <- lco$entries[which(sapply(lco$entries,function(x) x$title%in%text_randomize))]
  names(entry_randomize) <- 
    sapply(items_randomize, function(x) as.character(x$entry))
  
  ## Extract start time entry id
  item_time <- lco$entries[which(sapply(lco$entries,function(x) x$title%in%text_starttime))]
  if (is.null(text_starttime)) {
    entry_time <- NULL
  } else if (length(item_time)==0) {
    warning("No question matches with text_starttime. Ignored.")
    entry_time <- NULL
  } else if (length(item_time)>1) {
    warning("Multiple questions match with text_starttime. Only the first one is used.")
    entry_time <- item_time[[1]]$entry
  } else {
    entry_time <- item_time[[1]]$entry
  }

  ## Check weights_randomize
  if (is.null(weights_randomize)) {
    weights_randomize <- list()
    for (i in 1:length(entry_randomize)) {
      weights_randomize[[i]] <- NA
    }
  }
  if (is.numeric(weights_randomize)) {
    tmp <- weights_randomize
    weights_randomize = list()
    weights_randomize[[1]] <- tmp
    rm(tmp)
  } else if (!is.list(weights_randomize)) {
    stop("invalid weights_randomize class!")
  }
  if (length(weights_randomize)!=length(entry_randomize)) {
    stop("Invalid number of weights_randomize list elements!")
  }
  for (i in 1:length(weights_randomize)) {
    if (any(is.na(weights_randomize[[i]]))) {
      if (length(weights_randomize[[i]])>1) {
        warning("NAs in weights_randomize, not weighted.") 
        weights_randomize[[i]] <- NA
      }
    }
    if (!any(is.na(weights_randomize[[i]]))) {
      if (length(weights_randomize[[i]])!=length(entry_randomize[[i]])) {
        stop("weights_randomize length does not match with number of randomized conditions!") 
      }
      if (any(weights_randomize[[i]]<=0)) {
        stop("weights_randomize must be positive numbers!")
      }
    }
  }
  
  # Generate sample prefilled URL
  expurl <- paste0(lco$metadata$publishedUrl,"?usp=pp_url")
  if(!is.null(entry_time)) {
    expurl <- c(expurl,
                paste0("entry.",entry_time,"=' + currenttime + '"))
  }
  expurl <- c(expurl,
              paste0("entry.",names(entry_randomize),"=valuestobeassigned"))

  ## Function to generate Alphabet base based on number
  ## https://stackoverflow.com/questions/25876755/is-there-a-way-to-extend-letters-past-26-characters-e-g-aa-ab-ac
  extend <- function(alphabet) function(i) {
    base10toA <- function(n, A) {
      stopifnot(n >= 0L)
      N <- length(A)
      j <- n %/% N 
      if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
    }   
    vapply(i-1L, base10toA, character(1L), alphabet)
  }
  abcgen <- extend(c("a","b","c","d","e","f","g",
                     "h","i","j","k","l","m","n",
                     "o","p","q","r","s","t","u",
                     "v","w","x","y","z"))
  
  ## Java script to generate random conditions for each entry
  
  entry_vec <- character() # initiate random prefill line
  
  for(i in 1:length(entry_randomize)) {
    
    if (any(is.na(weights_randomize[[i]]))) {
      ## Java script to randomly choose one random element from vector
      entry_vec <- c(entry_vec, 
                     paste0("var vv",abcgen(i),
                            "=['",paste(entry_randomize[[i]], collapse="','"),
                            "'];var vv",abcgen(i),
                            "r=vv",abcgen(i),"[Math.floor(Math.random()*vv",
                            abcgen(i),".length>>0)];"))
    } else {
      
      ## Weighted sampling
      wr <- weights_randomize[[i]]
      for(k in 2:length(wr)) wr[k] = wr[k]+wr[k-1]
      wrsum <- wr[length(wr)]
      entry_vec <- c(entry_vec, 
                     paste0("var vv",abcgen(i),
                            "=['",paste(entry_randomize[[i]], collapse="','"),
                            "'];var wwr",abcgen(i),"=['",paste(wr, collapse="','"),
                            "'];var rnd=Math.random()*",wrsum,
                            ";for(var k=0;k<",length(wr),
                            ";k++){if(rnd<wwr",abcgen(i),"[k]){break;}}",
                            " var vv",abcgen(i),
                            "r=vv",abcgen(i),"[k];"))
      
    }
    
    if (i==length(entry_randomize)) {
      expurl <- gsub(paste0("entry\\.",names(entry_randomize)[[i]],"=.*$"), 
                  paste0("entry\\.",names(entry_randomize)[[i]],"=' + vv",
                         abcgen(i),"r"),
                  expurl)
    } else {
      expurl <- gsub(paste0("entry\\.",names(entry_randomize)[[i]],"=.*$"), 
                  paste0("entry\\.",names(entry_randomize)[[i]],"=' + vv",
                         abcgen(i),"r + '"),
                  expurl)
    }
    
  }
  
  ## Timezone Extraction
  if (timeZone=="survey_json timeZone") {
    timeZone <- lco$metadata$timeZone
    if (is.null(timeZone)) timeZone <- "Asia/Tokyo"
    cat(paste0("timeZone is set to ", timeZone,"\n"))
  }
  
  ## Modified based on https://httrksk2.github.io/RandomGen/
  ## Thanks also to this page: https://note.com/httrksk/n/nb1eeac4c9602
  # out <- paste0("<!DOCTYPE html><html><body><script>window.onload=function(){var currenttime = new Intl.DateTimeFormat('ja-JP', {dateStyle: 'medium',timeStyle: 'medium', timeZone: 'Asia/Tokyo',}).format(new Date());",paste0(entry_vec,collapse=""),"var urls=['",paste(expurl,collapse="&"),"];var weights=[1];var sumOfWeights=weights.reduce((a,b)=>a+b,0);var rnd=Math.random()*sumOfWeights;for(var i=0,sum=0;i<weights.length;i++){sum+=weights[i];if(rnd<sum){location.href=urls[i];break;}}}</script></body></html>")
  out <- paste0("<!DOCTYPE html><html><body><script>window.onload=function(){var currenttime = new Intl.DateTimeFormat('ja-JP', {dateStyle: 'medium',timeStyle: 'medium', timeZone: '",timeZone,"',}).format(new Date());",paste0(entry_vec,collapse=""),"var urls=['",paste(expurl,collapse="&"),"];for(var i=0;i<1;i++){location.href=urls[i];break;}}</script></body></html>")
  
  if (!is.null(file)) write(out, file) else return(out)
  
  }

#' Generate full-information codebook dataset
#'
#' @param survey_json Name of JSON survey file exported by gformexperiment custom add-on. 
#' @param DKtext Raw full text(s) of the response option(s) that indicate "don't know" answers.
#' @param DKcode Response code for "don't know" answers.
#' @param NAtext Raw full text(s) of the response option(s) that indicate refused answers.
#' @param NAcode Response code for refused answers.
#' @param text_randomize Raw full text(s) of the question(s) that are used to randomize experimental conditions. 
#' @param text_starttime Raw full text of the question that is used to record starting time.
#' 
#' @importFrom jsonlite read_json
#' @importFrom tidyr unnest
#' @importFrom varhandle check.numeric
#' @importFrom magrittr %>%
#' 
#' @export
gencodebook <- function(survey_json,
                        DKtext = "\u308f\u304b\u3089\u306a\u3044", 
                        DKcode = 888, 
                        NAtext = "\u7b54\u3048\u305f\u304f\u306a\u3044", 
                        NAcode = 999,
                        text_randomize = NULL,
                        text_starttime = NULL) {

  # require(jsonlite)
  # require(tidyr)
  # require(magrittr)
  # require(varhandle) # check.numeric()
  
  dcoall <- read_json(survey_json, simplifyVector = TRUE)$items
  
  ## Exclude PAGE_BREAK & SECTION_HEADER & IMAGE & VIDEO
  dco <- dcoall[which(!dcoall$type%in%c("PAGE_BREAK","SECTION_HEADER",
                                        "IMAGE","VIDEO")),]
  
  ## For SCALE variable, update choice values
  dco$choices[which(dco$type=="SCALE")] <- 
    lapply(which(dco$type=="SCALE"),
           function(i) as.character(seq(dco$lowerBound[i],dco$upperBound[i],by=1)))
  
  ## For GRID variable, columns are choice values
  dco$choices[which(dco$type%in%c("GRID","CHECKBOX_GRID"))] <- 
    dco$columns[which(dco$type%in%c("GRID","CHECKBOX_GRID"))]
  ### insert nonGRID variable rows as blank
  if ("rows"%in%names(dco)) {
    dco$rows[sapply(dco$rows, is.null)] <- ""
  } else {
    dco$rows <- ""
  }
  
  ## For TEXT variable, choice values are blank.
  dco$choices[which(dco$type%in%c("TEXT","PARAGRAPH_TEXT",
                                  "DATETIME","DATE",
                                  "DURATION","TIME",
                                  "FILE_UPLOAD"))] <- ""
  
  ## Make Values Numeric If checked to be numeric
  dco$choices <- lapply(dco$choices, function(x) if (all(check.numeric(x,exceptions=NULL))) as.numeric(x) else x)
  
  ## hasOtherOption update
  dco$hasOtherOption <- ifelse(dco$hasOtherOption%in%TRUE,1,0)
  
  ## isRequired update
  dco$isRequired <- ifelse(dco$isRequired%in%TRUE,1,0)
  
  ## Update Index
  loc_starttime <- which(dco$title%in%text_starttime)
  loc_randomize <- which(dco$title%in%text_randomize)
  loc_else <- which(!dco$title%in%c(text_starttime,text_randomize))
  if (!is.null(text_starttime)) {
    if(length(loc_starttime)==0) stop("text_starttime not found in survey_json!")
    if(length(loc_starttime)>1) stop("more than one text_starttime found in survey_json!")
    dco$index[loc_starttime] <- "timestamp_start"
  }
  if (!is.null(text_randomize)) {
    if(length(loc_randomize)==0) stop("text_randomize not found in survey_json!")
    dco$index[loc_randomize] <- 
      paste0("e", sprintf(paste0("%0",nchar(length(loc_else)),"d"),
                          seq(1,length(loc_randomize))))
  }
  dco$index[loc_else] <- 
    paste0("v", sprintf(paste0("%0",nchar(length(loc_else)),"d"),
                                   seq(1,length(loc_else))))
  
  ## Expand by GRID Rows
  dco <- dco %>% unnest(rows)
  ### update index
  for(k in unique(dco$index[dco$type=="GRID"])) {
    dco$index[which(dco$index==k)] <- 
      paste(k,sprintf(paste0("%0",nchar(length(which(dco$index==k))),"d"),
                      seq(1,length(which(dco$index==k)))),sep="_")
  }
  
  ## Assign Choice Codes
  dco$codes <- lapply(dco$choices, 
                      function(x) {
                        if (all(check.numeric(x,exceptions=NULL))) {
                          out <- as.numeric(x) 
                        } else if (any(x%in%c(DKtext,NAtext))) {
                          out <- ifelse(x%in%DKtext,DKcode,
                                        ifelse(x%in%NAtext,NAcode,NA))
                          out[is.na(out)] <- seq(1,length(out[is.na(out)]))
                        } else { 
                          out <- seq(1,length(x))
                        } 
                        out
                      })
  dco$codes[which(dco$choices=="")] <- NA
  
  ## Keep only necessary columns and rename columns
  dco <- dco[,c("index","id","type","isRequired","title","rows",
                "choices","codes","hasOtherOption")]
  colnames(dco) <- c("name","id","type","required","question","rows",
                     "choices","codes","hasOtherOption")
  
  return(dco)
  
}

#' Generate (and export) simplified codebook
#'
#' @param survey_json Name of JSON survey file exported by gformexperiment custom add-on. 
#' @param DKtext Raw full text(s) of the response option(s) that indicate "don't know" answers.
#' @param DKcode Response code for "don't know" answers.
#' @param NAtext Raw full text(s) of the response option(s) that indicate refused answers.
#' @param NAcode Response code for refused answers.
#' @param OtherOptiontext Texts in the codebook to indicate "other" answers.
#' @param OtherOptioncode Response code for "other" answers.
#' @param includeTimeStamp Boolean to indicate whether to include system-generated timestamp variable in the codebook or not. 
#' @param includeEmail Boolean to indicate whether to include system-generated email variable in the codebook or not. 
#' @param text_randomize Raw full text(s) of the question(s) that are used to randomize experimental conditions. 
#' @param text_starttime Raw full text of the question that is used to record starting time.
#' @param show_duration Boolean to indicate whether to include duration variable in the codebook or not (if <code>text_starttime</code> is available).
#' @param textout Name and path of the output html file to be saved.
#' @param csvout Name and path of the output csv file to be saved.
#' 
#' @importFrom utils write.csv
#'   
#' @export
gensimplecodebook <- function(survey_json,
                              DKtext = "\u308f\u304b\u3089\u306a\u3044", 
                              DKcode = 888, 
                              NAtext = "\u7b54\u3048\u305f\u304f\u306a\u3044", 
                              NAcode = 999,
                              OtherOptiontext = "\u305d\u306e\u4ed6",
                              OtherOptioncode = 666,
                              includeTimeStamp = TRUE,
                              includeEmail = FALSE,
                              text_randomize = NULL,
                              text_starttime = NULL,
                              show_duration = TRUE,
                              textout = NULL,
                              csvout = NULL) {
  
  dco <- gencodebook(survey_json, DKtext, DKcode, NAtext, NAcode,
                     text_randomize, text_starttime)
  
  ## If there is "Other" option
  dco$choices[which(dco$hasOtherOption==1)] <- lapply(which(dco$hasOtherOption==1), function(i) c(dco$choices[[i]],OtherOptiontext))
  dco$codes[which(dco$hasOtherOption==1)] <- lapply(which(dco$hasOtherOption==1), function(i) c(dco$codes[[i]],OtherOptioncode))
  
  ## Combine Choices & Codes
  dco$options <- sapply(1:nrow(dco), function(i) if (all(check.numeric(dco$choices[[i]],exceptions=NULL))) paste(dco$codes[[i]], collapse="; ") else paste(dco$choices[[i]],paste0("(",dco$codes[[i]],")"),collapse="; "))
  dco$options[which(dco$choices=="")] <- ""
  
  ## Keep only necessary columns and rename columns
  dco <- dco[,c("name","type","required","question","rows","options")]
  
  ## Include timestamp variable (if applicable)
  if (includeTimeStamp) {
    if ("timestamp_start"%in%dco$name & show_duration==TRUE) {
      dco <- 
        rbind(data.frame(name = "duration", type = "SYSTEM/TEXT", 
                         required = 1, 
                         question = "*Response time calcualated from timestamp and timestamp_start.",
                         rows = "", options = ""), dco)
    }
    dco <- 
      rbind(data.frame(name = "timestamp", type = "SYSTEM", 
                       required = 1, 
                       question = "*Auto recorded survey submission time.",
                       rows = "", options = ""), dco)
  }
  
  ## Include Email variable (if applicable)
  if (includeEmail) {
    if (read_json(survey_json)$metadata$collectsEmail) {
      dco <- 
        rbind(data.frame(name = "email", type = "SYSTEM", 
                         required = 1, 
                         question = "*Email addresses collected by the system.",
                         rows = "", options = ""), dco)
    } 
  }
  
  ## ID variable
  dco <- 
    rbind(data.frame(name = "id", type = "SYSTEM", 
                     required = 1, 
                     question = "*Unique respondent identifier.",
                     rows = "", options = ""), dco)
  
  ## Export to text file (if applicable)
  if (!is.null(textout)) {
    tmp <- as.list(as.data.frame(t(dco)))
    names(tmp) <- NULL
    tmp <- lapply(tmp, function(x) c(paste0(colnames(dco),": ", x),"",""))
    writeLines(unlist(tmp), textout)
    cat(paste("Written to",textout))
  }
  
  ## Export to csv file (if applicable)
  if (!is.null(csvout)) {
    write.csv(dco,"codebook.csv", row.names = FALSE)
    cat(paste("Written to",csvout))
  }
  
  ## Just show if not exported
  if (is.null(textout)&is.null(csvout)) return(dco)

}

#' Import Google Form survey CSV Data using JSON file information
#'
#' @param responses_data Name of either one of (1) JSON responses file exported by gformexperiment custom add-on or (2) CSV survey response file exported using Google Sheets output. 
#' @param survey_json Name of JSON survey file exported by gformexperiment custom add-on. 
#' @param DKtext Raw full text(s) of the response option(s) that indicate "don't know" answers.
#' @param DKcode Response code for "don't know" answers.
#' @param NAtext Raw full text(s) of the response option(s) that indicate refused answers.
#' @param NAcode Response code for refused answers.
#' @param OtherOptiontext Texts in the codebook to indicate "other" answers.
#' @param OtherOptioncode Response code for "other" answers.
#' @param text_randomize Raw full text(s) of the question(s) that are used to randomize experimental conditions. 
#' @param text_starttime Raw full text of the question that is used to record starting time.
#' @param includeTimeStamp Boolean to indicate whether to include system-generated timestamp variable in the codebook or not. 
#' @param show_duration Boolean to indicate whether to include duration variable in the codebook or not (if <code>text_starttime</code> is available).
#' @param includeEmail Boolean to indicate whether to include system-generated email variable in the codebook or not. 
#' @param survey_csv (deprecated) import responses data in CSV file (incorporated by <code>responses_data</code>).
#' @param ... Optional argumens passed to <code>read.csv()</code> function.
#'  
#' @importFrom labelled set_variable_labels
#' @importFrom labelled set_value_labels
#' @importFrom dplyr tibble
#' @importFrom utils read.csv
#' @importFrom jsonlite read_json
#' 
#' @export
read_gform <- function(responses_data,
                       survey_json,
                       DKtext = "\u308f\u304b\u3089\u306a\u3044", 
                       DKcode = 888, 
                       NAtext = "\u7b54\u3048\u305f\u304f\u306a\u3044", 
                       NAcode = 999,
                       OtherOptiontext = "\u305d\u306e\u4ed6",
                       OtherOptioncode = 666,
                       text_randomize = NULL,
                       text_starttime = NULL,
                       includeTimeStamp = TRUE,
                       show_duration = TRUE,
                       includeEmail = FALSE,
                       survey_csv = NULL, 
                       ...) {
  
  # require(labelled)
  # require(dplyr)
  # require(utils)
  
  ## 
  if (missing(responses_data) & is.null(survey_csv)) {
    stop("Provide responses_data!")
  } else if (missing(responses_data) & !is.null(survey_csv)) {
    responses_data <- survey_csv
    warning("survey_csv is deprecated. Use responses_data instead.")
  } 

  ## Import survey_json
  dco <- gencodebook(survey_json,
                     DKtext,DKcode,NAtext,NAcode,
                     text_randomize, text_starttime)
  
  if (read_json(survey_json)$metadata$collectsEmail) {
    setcolnames <- c("timestamp","email",dco$name)
    setidnames <- c("timestamp","email",dco$id)
    gettypenames <- c("SYSTEM","SYSTEM",dco$type)
  } else {
    setcolnames <- c("timestamp",dco$name)
    setidnames <- c("timestamp",dco$id)
    gettypenames <- c("SYSTEM",dco$type)
  }
  
  ## If importing CSV file downloaded from Google Sheets
  if (grepl("\\.csv$", responses_data)) {
    
    ## Import variable names from survey_csv
    dnames <- read.csv(responses_data, header=FALSE, ...)[1,]
    
    ## Import survey_csv
    d <- read.csv(responses_data, na.strings = "", ...)
    
    ## Exclude columns with missing labels 
    d <- d[,which(!is.na(dnames))]
    # print(ncol(d)); print(nrow(dco))
    
    ## Check Data Structure and replace column names
    ### (in the case of emails collected by the system)
    if ("email"%in%setcolnames) {
      
      if(ncol(d)!=nrow(dco)+2) stop("# of survey_csv columns does not match with # of variables generated from survey_json!")
      
      if (!all(sapply(1:nrow(dco), function(i) grepl(dco$question[i], dnames[i+2])))) stop("survey_csv column names do not match with question texts in survey_json!")
      
      colnames(d) <- setcolnames
      
      ### (in the case of emails NOT collected by the system)
    } else {
      
      if(ncol(d)!=nrow(dco)+1) stop("# of survey_csv columns does not match with # of variables generated from survey_json!")
      
      if (!all(sapply(1:nrow(dco), function(i) grepl(dco$question[i], dnames[i+1])))) stop("survey_csv column names do not match with question texts in survey_json!")
      
      colnames(d) <- setcolnames
      
    }
    
  ## If importing responses json file exported using surveyexport.js.
  } else if (grepl("\\.json$", responses_data)) {
    
    ## Read in JSON data
    d <- read_json(responses_data)
    
    ## Identify grid question locations, if any
    getgridloc <- grep("GRID", gettypenames)
    ## Extract grid sub question numbers, if any
    if (length(getgridloc)>0) {
      setgridqloc <- as.numeric(gsub("^.*_","",setcolnames[getgridloc]))
    }
    
    ## Extract responses by respondents
    genresp <- function(RESP) {
      
      # Each response
      rech <- sapply(RESP, function(x) x$response)[
        match(setidnames, sapply(RESP, function(x) x$id))
      ]
      
      # Assign grid responses
      if(length(getgridloc)>0) {
        if (all(getgridloc==which(sapply(rech, length)>1))) {
          rech[getgridloc] <- 
            lapply(1:length(getgridloc), 
                   function(i) rech[[getgridloc[i]]][[setgridqloc[i]]])  
        } else {
          stop("Something is wrong with GRID/CHECKBOX_GRID data!")
        }
      }
      
      # Change to NA if NULL
      rech <- lapply(rech, function(x) if (is.null(x)) NA else x)
      
      # Return output
      if (all(sapply(rech, length)==1)){
        unlist(rech)
      } else {
        stop("Questions with more than one responses!")
      }
    }
    
    ## Generate dataset
    d <- as.data.frame(t(sapply(d, genresp)))
    colnames(d) <- setcolnames
    
  }

  ## Data to export
  dout <- tibble(id = 1:nrow(d))
  
  ## Add Email
  if ("email"%in%colnames(d) & includeEmail==TRUE) {
    dout$email <- as.character(d$email)
  }
  
  ## Add timestamp
  if (includeTimeStamp==TRUE) {
    dout$timestamp <- as.character(d$timestamp)
    if ("timestamp_start"%in%names(d) & show_duration==TRUE) {
      dout$duration <- 
        as.POSIXct(d$timestamp) - as.POSIXct(d$timestamp_start)
    }
  } 

  ## Generate Variables
  for(i in 1:nrow(dco)) {
    ## If Check Box Questions, Create Dummy Variables for Each Category
    if (dco$type[i]%in%c("CHECKBOX","CHECKBOX_GRID")) {
      for(j in 1:length(dco$codes[[i]])) {
        ## Create Dummy
        dout[,paste0(dco$name[i],"_box",dco$codes[[i]][j])] <- 
          sapply(d[,dco$name[i]], function(x) any(x%in%dco$choices[[i]][j])*1)
        ## Variable label
        tmplab <- dco$question[i]
        if(dco$rows[i]!="") tmplab <- paste(tmplab, "row:", dco$rows[i])
        tmplab <- paste(tmplab, "option:", dco$codes[[i]][j])
        dout[,paste0(dco$name[i],"_box",dco$codes[[i]][j])] <- 
          set_variable_labels(
            dout[,paste0(dco$name[i],"_box",dco$codes[[i]][j])],
            .labels = tmplab)
      }
      if (dco$hasOtherOption[i]==1) {
        ## Dummy
        dout[,paste0(dco$name[i],"_box",OtherOptioncode)] <- 
          sapply(d[,dco$name[i]], function(x) any(!x%in%dco$choices[[i]])*1)
        ## Variable label
        tmplab <- dco$question[i]
        if(dco$rows[i]!="") tmplab <- paste(tmplab, "row:", dco$rows[i])
        tmplab <- paste(tmplab, "option:", dco$codes[[i]][j])
        dout[,paste0(dco$name[i],"_box",OtherOptioncode)] <- 
          set_variable_labels(
            dout[,paste0(dco$name[i],"_box",OtherOptioncode)],
            .labels = tmplab)
        ## Free Answer
        dout[,paste0(dco$name[i],"_txt",OtherOptioncode)] <- 
          sapply(d[,dco$name[i]], function(x) if (all(x%in%dco$choices[[i]])) {
            NA
          } else {
            paste(x[which(!x%in%dco$choices[[i]])],collapse=";")
          })
        tmplab <- paste(tmplab, "(text)")
        dout[,paste0(dco$name[i],"_txt",OtherOptioncode)] <-
          set_variable_labels(
            dout[,paste0(dco$name[i],"_txt",OtherOptioncode)], 
            .labels = tmplab)
      }
    ## If Multiple Choice/List/Scale Questions, 
    } else if (dco$type[i]%in%c("MULTIPLE_CHOICE","LIST",
                                "GRID","SCALE")) {
      ## Without OtherOption:
      if (dco$hasOtherOption[i]==0) {
        ## Main variable
        dout[,dco$name[i]] <- 
          dco$codes[[i]][match(d[,dco$name[i]],dco$choices[[i]])]
        ## Set labels if labels different from values
        if (!all(dco$choices[[i]]==dco$codes[[i]])) {
          tmplab <- dco$codes[[i]]
          names(tmplab) <- dco$choices[[i]]
          dout[,dco$name[i]] <- 
            set_value_labels(dout[,dco$name[i]], .labels = tmplab)
        }
      ## With OtherOption: 
      } else {
        ## Main variable
        dout[,dco$name[i]] <- 
          ifelse(is.na(d[,dco$name[i]]),NA,
                 ifelse(!d[,dco$name[i]]%in%dco$choices[[i]],OtherOptioncode,
                        dco$codes[[i]][match(d[,dco$name[i]],dco$choices[[i]])]))
        ## Set labels
        tmplab <- c(dco$codes[[i]],OtherOptioncode)
        names(tmplab) <- c(dco$choices[[i]],OtherOptiontext)
        dout[,dco$name[i]] <- 
          set_value_labels(dout[,dco$name[i]], .labels = tmplab)
        ## OtherOption open-ended answers
        dout[,paste0(dco$name[i],"_txt",OtherOptioncode)] <- 
          ifelse(dout[,dco$name[i]]%in%OtherOptioncode,d[,dco$name[i]],NA)
        ## Variable label for open-ended answer
        tmplab <- paste(dco$question[i], "option:", OtherOptiontext, "(text)")
        dout[,paste0(dco$name[i],"_txt",OtherOptioncode)] <-
          set_variable_labels(dout[,paste0(dco$name[i],"_txt",OtherOptioncode)],
                              .labels = tmplab)
      }
      ## Variable label
      tmplab <- dco$question[i]
      if(dco$rows[i]!="") tmplab <- paste(tmplab, "row:", dco$rows[i])
      dout[,dco$name[i]] <- 
        set_variable_labels(dout[,dco$name[i]], .labels = tmplab)
    ## If Other Response Format
    } else {
      dout[,dco$name[i]] <- as.character(d[,dco$name[i]])
      ## Variable label
      dout[,dco$name[i]] <- 
        set_variable_labels(dout[,dco$name[i]], .labels = dco$question[i])
    }
  }
  
  return(dout)
  
}



#' Combine multiple variables
#' 
#' @param data data.frame object 
#' @param vnames chracter vector of variable names to combine.
#' @param keep_var_label Boolean to indicate whether to keep variable label (of the first varaible in <code>vnames</code>) or not.
#' @param keep_val_labels Boolean to indicate whether to keep value labels (of the first varaible in <code>vnames</code>) or not.
#' @param reversed_loc Integer vector to indicate the location of variables to be reversed in <code>vnames</code>.
#' @param val_lim Integer vector of length 2 that indicates the limits of values (only used when <code>reversed_loc</code> is not NULL.)
#'  
#' @importFrom labelled set_variable_labels
#' @importFrom labelled set_value_labels
#' @importFrom labelled var_label
#' @importFrom labelled val_labels
#' 
#' @export
bindquestions <- function(data, vnames,
                          keep_var_label = FALSE,
                          keep_val_labels = TRUE,
                          reversed_loc = NULL,
                          val_lim = NULL) {
  
  ## output 
  data$vout <- rep(NA, nrow(data))

  ## insert values
  if (is.null(reversed_loc)|is.null(val_lim)) {
    for(i in 1:length(vnames)) {
      data$vout[!is.na(data[,vnames[i]])] <- 
        unlist(data[!is.na(data[,vnames[i]]),vnames[i]])
    }
  } else {
    for(i in 1:length(vnames)) {
      if (length(val_lim)!=2) stop("Length of val_lim must be 2!")
      if (i %in% reversed_loc) {
        data$vout[!is.na(data[,vnames[i]])] <- 
          sum(val_lim) - unlist(data[!is.na(data[,vnames[i]]),vnames[i]])
        cat(paste0(vnames[i]," is reversed.\n"))
      } else {
        data$vout[!is.na(data[,vnames[i]])] <- 
          unlist(data[!is.na(data[,vnames[i]]),vnames[i]])
      }
    }
  }
  
  if (keep_var_label==TRUE) {
    # require(labelled)
    data$vout <- 
      set_variable_labels(data$vout, 
                          .labels=var_label(data[,vnames[1]])[[1]])
  }
  if (keep_val_labels==TRUE) {
    # require(labelled)
    if (!is.null(val_labels(data[,vnames[1]])[[1]])) {
        data$vout <- 
          set_value_labels(data$vout, 
                           .labels=val_labels(data[,vnames[1]])[[1]])
    }
  }
  
  return(data$vout) 
  
}

