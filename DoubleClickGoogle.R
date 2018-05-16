# setwd("~/Documents/FLAGCX/Projects/DoubleClickBM")

## Run functions first to be able to access the API and retrieve data 

# Enabling OAUTH 
DCAuth <- function(client.id,client.secret,runwebserver=FALSE){
  
  app.dfa <- oauth_app("google",client.id,client.secret)
  if(runwebserver) {
    app.token <- oauth2.0_token(oauth_endpoints("google"), app.dfa,
                                scope = c("https://www.googleapis.com/auth/dfareporting","https://www.googleapis.com/auth/devstorage.read_only"),
                                cache=FALSE,options(httr_oob_default=TRUE))
  } else {
    app.token <- oauth2.0_token(oauth_endpoints("google"), app.dfa,
                                scope = c("https://www.googleapis.com/auth/dfareporting","https://www.googleapis.com/auth/devstorage.read_only"),
                                cache=FALSE)
  }
  
  DC.token <<- app.token
  DC.authmethod <<- 'oauth'
  
}


# Authorize with the API
client.id <- "yourclientid.apps.googleusercontent.com"
client.secret <- "yourclientsecret"

DCAuth(client.id,client.secret)

# Function to get available profiles and find your profile ID
userprofiles.list <- function(client.id,client.secret){
  
  req.url <- "https://www.googleapis.com/dfareporting/v3.0/userprofiles"
  response <- fromJSON(api.request(req.url))
  
  return(response$items)
  
}


# Function to get available reports and find your report ID
reports.list <- function(profileId, results=10, scope='', sortField='', sortOrder='', fields='', delay=2){
  
  # build query string
  if(results>=10) {
    maxResults <- 10
  }
  q.string <- paste0("maxResults=",maxResults)
  if(nchar(scope)) {
    q.string <- paste0(q.string,"&scope=",scope)
  }
  if(nchar(sortField)) {
    q.string <- paste0(q.string,"&sortField=",sortField)
  }
  if(nchar(sortOrder)) {
    q.string <- paste0(q.string,"&sortOrder=",sortOrder)
  }
  if(nchar(fields)) {
    q.string <- paste0(q.string,"&fields=",fields)
  }
  
  report <- data.frame()
  workingResultNum <- 0
  pageToken <- ""
  hasNextPage <- TRUE
  while((workingResultNum<results)&&hasNextPage==TRUE) {
    if(nchar(pageToken)) {
      req.q.string <- paste0(q.string,"&pageToken=",pageToken)
    } else {
      req.q.string <- q.string
    }
    req.url <- paste0("https://www.googleapis.com/dfareporting/v3.0/userprofiles/",profileId,"/reports")
    response <- api.request(req.url,querystring=req.q.string)
    response <- fromJSON(response)
    
    # kill complex nested columns as rbind.fill doesn't handle them
    # criteria, pathToConversionCriteria, schedule, delivery
    
    response$items$criteria <- NULL
    response$items$reachCriteria <- NULL
    response$items$floodlightCriteria <- NULL
    response$items$crossDimensionReachCriteria <- NULL
    response$items$pathToConversionCriteria <- NULL
    response$items$schedule <- NULL
    response$items$delivery <- NULL
    
    if(nrow(report)>0) {
      report <- rbind.fill(report,data.frame(response$items))
    } else {
      report <- data.frame(response$items)
    }
    
    pageToken <- response$nextPageToken
    workingResultNum <- nrow(report)
    if(nchar(pageToken)){
      hasNextPage <- TRUE
    } else {
      hasNextPage <- FALSE
    }
    
    Sys.sleep(delay)
    
  }
  
  return(report)
  
}


# Function to get file list
files.list <- function(profileId, reportId, results=10, scope='', sortField='', sortOrder='', fields='', delay=2){
  
  # build query string
  if(results>=10) {
    maxResults <- 10
  }
  q.string <- paste0("maxResults=",maxResults)
  if(nchar(scope)) {
    q.string <- paste0(q.string,"&scope=",scope)
  }
  if(nchar(sortField)) {
    q.string <- paste0(q.string,"&sortField=",sortField)
  }
  if(nchar(sortOrder)) {
    q.string <- paste0(q.string,"&sortOrder=",sortOrder)
  }
  if(nchar(fields)) {
    q.string <- paste0(q.string,"&fields=",fields)
  }
  
  report <- data.frame()
  workingResultNum <- 0
  pageToken <- ""
  hasNextPage <- TRUE
  while((workingResultNum<results)&&hasNextPage==TRUE) {
    if(nchar(pageToken)) {
      req.q.string <- paste0(q.string,"&pageToken=",pageToken)
    } else {
      req.q.string <- q.string
    }
    
    req.url <- paste0("https://www.googleapis.com/dfareporting/v3.0/userprofiles/",profileId,"/reports/",reportId,"/files")
    response <- api.request(req.url,querystring=req.q.string)
    response <- fromJSON(response)
    
    # Flatten the data frame for binding
    response$items$startDate <- response$items$dateRange$startDate
    response$items$endDate <- response$items$dateRange$endDate
    response$items$dateRange <- NULL
    response$items$browserUrl <- response$items$urls$browserUrl
    response$items$apiUrl <- response$items$urls$apiUrl
    response$items$urls <- NULL
    
    if(nrow(report)>0) {
      report <- rbind.fill(report,data.frame(response$items))
    } else {
      report <- data.frame(response$items)
    }
    
    pageToken <- response$nextPageToken
    workingResultNum <- nrow(report)
    if(nchar(pageToken)){
      hasNextPage <- TRUE
    } else {
      hasNextPage <- FALSE
    }
    
    Sys.sleep(delay)
    
  }
  
  return(report)
  
}


# API Request Function
api.request <- function(url, querystring="",method="GET"){
  
  if(DC.authmethod=='oauth') {
    if(method=="GET") {
      req <- GET(url,query=querystring,config(token = DC.token))
    } else if (method=="POST") {
      # requires a dummy body to avoid a content-length error
      req <- POST(url=paste0(url,'?',querystring),config=config(token = DC.token),body=list('dummy'),encode='json')
    }
  }
  
  stop_for_status(req)
  response <- content(req,as='text')
  
  return(response)
}


# Function Get Files 
files.get <- function(fileId, reportId){
  
  req.url <- paste0("https://www.googleapis.com/dfareporting/v3.0/reports/",reportId,"/files/",fileId)
  response <- api.request(req.url,querystring="alt=media")
  
  # we have to remove parts of this text to get a readable table
  # first remove everything prior to the report field definitions (header row)
  response <- str_split(response,"Report Fields\n")[[1]][2]
  # then remove the totals row
  response <- str_split(response,"\nGrand Total")[[1]][1]
  
  report <- read.csv(text=response,header=TRUE,sep=",")
  
  return(report)
  
}

