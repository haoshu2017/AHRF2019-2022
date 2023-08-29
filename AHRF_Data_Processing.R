########################################################################
####### This program: 1. Download the AHRF county-level Data (2019-2022)
###################### 2. Process variables of interests
####### Programmer/Author: HD 08/26/2023

########################################################################







## Set-up --------------------------------------------------------------
library(tidyverse)
library(readxl)
library(labelled)


getwd()

dir.create("/Users/haoshu/AHRF/county_clean")
data_out<-"/Users/haoshu/AHRF/county_clean"

data_raw<-"/Users/haoshu/AHRF/county_raw/"

## Download raw files -------------------------------------------------

url<-c("https://data.hrsa.gov//DataDownload/AHRF/AHRF_2019-2020.ZIP",
       "https://data.hrsa.gov//DataDownload/AHRF/AHRF_2020-2021.ZIP",
       "https://data.hrsa.gov//DataDownload/AHRF/AHRF_2021-2022.ZIP")

folder<-c("AHRF_2019-2020",
          "AHRF_2020-2021",
          "AHRF_2021-2022")

file_zip = tempfile(fileext = ".zip")

# Note: Downloading in a loop from 2019-2022
for (i in seq_along(url)) {
        
        download.file(url[i], file_zip)
        dir.create(paste0(data_raw, folder[i]))
        zip_folder <-paste0(data_raw, folder[i])
        unzip(file_zip, exdir = zip_folder, junkpaths = T)
        
        list.files(zip_folder)
        
}


## Prepare the Data files --------------------------------------------------

raw_src<-vector(mode = 'list', length = 3)
doc_src<-vector(mode = 'list', length = 3)
ahrf_county<-vector(mode = 'list', length = 3)
ahrf_county_layout<-vector(mode = 'list', length = 3)
data<-c('ahrf2020.asc','ahrf2021.asc', 'ahrf2022.asc')

for (i in seq_along(data)) {
        raw_src[[i]] = paste0(data_raw, folder[i], "/", data[i]) # Raw data
        doc_src[[i]] = paste0(data_raw, folder[i], "/",folder[i], " Technical Documentation.xlsx")
        
        #' readxl::read_excel(doc_src) %>% View
        
        # Find out the line for the first field: F00001 ---------------------------
        # read_excel(doc_src) %>% View()
        
        read_excel(doc_src[[i]]) %>%
                pull(...1) %>%
                grepl("F00001", .) %>%
                which() -> bgn_line
        bgn_line
        
        # Prepare the layout file -------------------------------------------------
        
        ahrf_county_layout[[i]]<-read_excel(doc_src[[i]],
                                            col_names = c("field", "col_col", "year_of_data", "var_label",
                                                          "characteristics", "source", "date_on"),
                                            skip = bgn_line) %>%
                ## All filed starts with F and then some number
                filter(grepl("^F[0-9]", field)) %>%
                separate(col_col, c("col_start", "col_end")) %>%
                mutate_at(c("col_start", "col_end"), as.integer) 
        
        ahrf_county_layout[[i]]
        
        # Prepare the county AHRF file --------------------------------------------
        
        read_fwf(file = raw_src[[i]],
                 col_positions = fwf_positions(start = ahrf_county_layout[[i]]$col_start,
                                               end = ahrf_county_layout[[i]]$col_end,
                                               col_names = ahrf_county_layout[[i]]$field)) -> ahrf_county[[i]]
        ahrf_county[[i]]
        
        # Add variable labels -----------------------------------------------------
        
        ahrf_county_layout[[i]] %>%
                select(field, var_label) %>%
                deframe() %>%
                as.list() -> var_label(ahrf_county[[i]])
        
}
# Save it ----------------------------------------------------------------
ahrf_county_2020<-ahrf_county[[1]]
ahrf_county_2021<-ahrf_county[[2]]
ahrf_county_2022<-ahrf_county[[3]]




# Delete raw data as it’s too large ---------------------------------------
unlink(raw_src)

write.xlsx(ahrf_county, paste0(data_out, "/ahrf_county_2019-2020.xlsx"))