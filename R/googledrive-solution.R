library(googledrive)
library(googlesheets4)
library(dplyr)
library(purrr)
#library(sendmailR)
#library(blastula)
library(gmailr)


## Authentication
email_auth <- rstudioapi::askForSecret(name = "gmail_adress", message = "Gmail address")

googledrive::drive_auth(email = email_auth)
googlesheets4::gs4_auth(email= email_auth)

## Auth Gmail API, for setup of Gmail API, see: https://gmailr.r-lib.org/articles/oauth-client.html
list_auth <- list()
list_auth$path_auth <- rappdirs::user_data_dir("gmailr")
list_auth$file_json <- list.files(pattern = ".json")

if (!dir.exists(list_auth$path_auth)) dir.create(list_auth$path_auth)

list_auth$file_auth <- list.files(path = list_auth$path_auth, pattern = ".json")

if (length(list_auth$file_auth) > 1) {
  unlink(list.files(list_auth$path_auth, full.names = T), recursive = T) 
} else if (length(list_auth$file_auth) == 1 & list_auth$file_json != list_auth$file_auth) {
  unlink(list.files(list_auth$path_auth, full.names = T), recursive = T)
} 

list_auth$file_auth_confirm <- list.files(path = list_auth$path_auth, pattern = ".json")

if (length(list_auth$file_auth_confirm) == 0) file.copy(list_auth$file_json, list_auth$path_auth)

gm_auth_configure()
gmailr::gm_auth(email = email_auth)

## Test
print(gm_profile())
rm(list_auth)

## get data
# googledrive::drive_find(pattern = "OF-missions")
# googledrive::drive_get("OF-missions/Mission form (Responses)")$id
# googledrive::drive_get("OF-missions/Mission form for analysis (Approved)")$id

gg_dat <- list()
gg_dat$df_survey   <- read_sheet(ss = "11VUpgEndIu-M8CfaA3XjZxpglBhrHxCJ8PCv5H-mFiM")
gg_dat$df_approved <- read_sheet(ss = "186UF16HA9TSxq_N4tb3kACgE-uzoMnH5oDL3ChB1Y8k")

gg_dat$nrow_survey   <- nrow(gg_dat$df_survey)
gg_dat$nrow_approved <- nrow(gg_dat$df_approved)

## Get new data to approval form
if (gg_dat$nrow_survey > gg_dat$nrow_approved) {
  
  gg_dat$df_add <- gg_dat$df_survey |> dplyr::slice_tail(n = gg_dat$nrow_survey - gg_dat$nrow_approved)
  gg_dat$df_add$`Editing status`	<- "First sync"
  gg_dat$df_add$`Editing timestamp` <- NA	
  gg_dat$df_add$`Submission status` <- "Not submitted"
  gg_dat$df_add$`Submission timestamp` <- NA
  gg_dat$df_add$`Approval status` <- "Not approved"
  gg_dat$df_add$`Approval timestamp` <- NA
}

## Test
tt <- gg_dat$df_add

## Send email for approval to boss
x = 1
gg_dat$df_add2 <- map(1:nrow(gg_dat$df_add), function(x){
  
  ## Create separate data for each new report
  
  ## Generate PDF based on new data
  
  ## Save PDF in local and ggdrive
  
  ## Send email with PDF
  from <- email_auth
  to   <- gg_dat$df_add$`Supervisor email`[x]
  subject <- paste(
    "Approval mission of", gg_dat$df_add$`Reporting Officer`[x], "submitted on:",
    lubridate::date(gg_dat$df_add$Timestamp[x]))
  email <- gm_mime() |>
    gm_to(to) |>
    gm_from(from) |>
    gm_subject(subject) |>
    gm_text_body(glue::glue(
      "Dear ", gg_dat$df_add$`Supervisor name`[x], ",
      
      Please find attached the mission's report of ", gg_dat$df_add$`Reporting Officer`[x],
      " for you review and approval please.
      
      Email sent automatically from SERVICE NAME."
    ))
  
  ## Check msg
  strwrap(as.character(email))
  
  ## Send
  gm_send_message(email)
  
  ## Update Data
  gg_dat$df_add$approval_sent[x]	<- "Submitted"
  gg_dat$df_add$approval_sent_timestamp[x] <- Sys.time()
  
  gg_dat$df_add[x,]
  
}) |> list_rbind()

gg_dat$df_add2$approval_sent_timestamp <- lubridate::as_datetime(gg_dat$df_add2$approval_sent_timestamp)
tt <- gg_dat$df_add2


