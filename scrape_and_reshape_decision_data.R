# reshapes metadata to fit expected form
# identify duplicate decisions (by "file_name", which contains the case id -- in some cases the duplicates will just be id,
# other cases metadata will overlap but decisions will be different, other cases decisions will overlap but metadata will be
# different, and still others data will be fully duplicated) to save for review
# saves duplicate decisions with metadata in a spreadsheet for review, saves metadata with corresponding file_names for documents
# with no associated problems and saves these decisions to .txt files of those names, adds filenames with no associated problems
# to the running filenames list (file_names_list_path)
saveTexts<-function(spreadsheetdir,setnumberdir,file_names_list_path){
  library(dplyr)
  library(tidyr)
  library(tidyverse)
  df<-read.csv(spreadsheetdir,header=F)
  metadata<-tibble(df)
  #reshape metadata
  metadata<-metadata %>%
    mutate(id = row_number()) %>%
    gather(key="col",value="val",-"V1",-"V9",-"id") %>%
    filter(str_detect(val,"^Classe")|str_detect(val,"^Assunto")|str_detect(val,"^Magistrado")|str_detect(val,"^Comarca")|str_detect(val,"^Foro")|str_detect(val,"^Vara")|str_detect(val,"^Data"))%>%
    select(-col) %>%
    separate(val,c("class","value"),":") %>%
    mutate(class = str_trim(class)) %>%
    mutate(value = str_trim(value)) %>%
    spread(class,value) %>%
    rename(file_name=V1,decision=V9) %>%
    select(-id) %>%
    distinct() 
  
  # load running list of filenames already saved to avoid overwriting
  filenamessofar<-read.csv(file_names_list_path,header=T)
  filenametoadd<-select(metadata,file_name)
  duplicates<-intersect(filenamessofar,filenametoadd)
  if (anyDuplicated(bind_rows(filenametoadd,filenamessofar))==0) {
    # if no duplications with previously saved files or other files in the current set, just save metadata to info spreadsheet,
    # save decisions to files with file_name, and save file_names to running list 
    write_csv(select(metadata,-decision), paste(setnumberdir,"info.csv",sep=""), append=T, col_names = T)
    mapply(function(x,y) write(x,y), metadata$decision, metadata$file_name)
    write_csv(select(metadata,file_name), file_names_list_path, append=T, col_names = F)
  } else {
    isduplicate<-unlist(lapply(duplicated(metadata$file_name), function(x) toString(x)))
    metadata<-metadata %>%
      add_column(isduplicate)
    # filter out duplicated rows
    handcheck<- metadata %>%
      filter(str_detect(isduplicate,"TRUE") | file_name %in% duplicates)
    writeable<-setdiff(metadata,handcheck)
    # write non duplicated row metadata to info spreadsheet, save decisions to files with file_name, and 
    # save file_names to running list
    write_csv(select(writeable,-decision,-isduplicate), paste(setnumberdir,"info.csv",sep=""), append=T, col_names = T)
    write_csv(select(writeable,file_name), file_names_list_path, append=T, col_names = F)
    mapply(function(x,y) write(x,y), writeable$decision, writeable$file_name)
    handcheck<-as.data.frame(handcheck)
    # save data to "handcheck" to duplicates file
    write_csv(handcheck,paste(setnumberdir,"check.csv",sep=""),append=T,col_names=T)
  }  
}

#sub function to parse the html of the parts of the page with the embedded decision text
#takes the block of the page with this html and an metadata.filepath to save the metadata + decisions in csv form
#for more robust duplicate checking in saveTexts above
parseDecisions<-function(decision.block, metadata.filepath){
  
  tryCatch(
    {
      #parse the html using hierarchical nodes
      page.parse<-htmlTreeParse(decision.block, useInternalNodes = T, encoding="UTF-8")
      #the decisions are contained in <tr> tags with the attribute class='fundocinza1 - this retrieves each decision as a separate node
      decision.nodes<-getNodeSet(page.parse, "//*/tr[@class='fundocinza1']")
      #for each node, get the internal elements contained in span tags
      all.sub.nodes<-lapply(decision.nodes, function(x) xmlElementsByTagName(x, "span", recursive=T))
      #the first span tag is the unique number - uses this as the filename
      all.titles<-lapply(all.sub.nodes, function(x) xmlValue(x[[1]],trim=T))
      #the third span tag is the decision text - get all of these
      all.texts<-lapply(all.sub.nodes, function(x) xmlValue(x[[3]]))
      
      #metadata
      all.metadata<-lapply(decision.nodes, function(x) xmlElementsByTagName(x, "td", recursive=T))
      a<-(lapply(all.metadata, function(x) xmlValue(x[4:length(x)],recursive=T,trim=T)))
      
      library(tidyverse)
      a<-tibble(a)
      a<-unnest_wider(a,1)
      
      #wrong!
      all.lengths<-lapply(all.texts, function(x) toString(nchar(x[[1]])))
      
      id<-paste(unlist(all.titles), ".", unlist(all.lengths), ".txt", sep="")
      metadata<-add_column(a,id,.before=1)
      #metadata<-data.frame(id,a,b,c,d,e,f,g,h,stringsAsFactors = F)
      #write.table(metadata, file = metadata.filepath, append=T, sep=",", dec=".", row.names=F, col.names=F, qmethod="double")
      write_csv(metadata,metadata.filepath,na="",append=T,col_names=F)
    }, 
    error=function(e){
      try(print(id))
      print("error")
    }
  )
  
}

#this is the primary function to crawl the pages
#it requires a starting url (after this it will just follow the next page button), as well as an output folder
#max.pages is one way to control how much you scrape - the program will just keep going to the next page until either
#the current page number is greater than max pages OR there is no "next page" button
#the default - max.pages=100 - means that it will scarape 100 pages. It is useful to set this low if you want to test the function
#this also depends on the R packages RSelenium and XML to run so make sure that you have those installed
#The code below was provided by Dr. Mark Algee-Hewitt for me to build upon
crawlESAJ<-function(url, output.folder, metadata.filename, max.pages=100){
  
  metadata.filepath<-paste(output.folder,metadata.filename,sep="/")
  
  library(RSelenium)
  library(XML)
  #start the selenium server using the chrome browser (you can change this to "firefox" or another browser name)
  #note that this will open a new chrome/browser window on your computer controlled by the script - you can watch it to
  #see your progress - just don't close it while the script is running
  rD<-rsDriver(browser="firefox")
  #create the client
  remDr<-rD[["client"]]
  #navigate to the starting url
  remDr$navigate(url)
  
  #set the flags to tell the script when to stop (it will run until collect.page==F)
  collect.page<-T
  current.page<-1
  
  #begin the loop
  while(collect.page){
    #the html table with the decision texts are in an set of div tags with an id attribute equal to "divDadosResultado"
    #this selects all of those elements (really only one per page)
    tryCatch(
      {
        text.blocks<-remDr$findElements("id", "divDadosResultado")
        #this grabs the html from the decision block
        sections<-lapply(text.blocks, function(x) x$getElementAttribute("outerHTML"))
      }, 
      error=function(e){
        rD[["server"]]$stop()
        return("Error")
      }
    )
    #this runs the above function to parse the decisions and save the results
    parseDecisions(unlist(sections), output.folder, metadata.filepath)
    #if you have scraped the max number of pages, set the collect.page flag to F and stop
    if(current.page>=max.pages){
      collect.page<-F 
    } else {
      #if not, then find the next page button - wrapped in an error handler in case there is no next page button (in which case you have scraped all of the decisions )
      nextPageSearch<-tryCatch(
        {
          nextPageButton<-remDr$findElement("link text", ">")
        }, 
        error=function(e){
          rD[["server"]]$stop()
          return("Error")
        }
      )
      #check if the next page button returned an S4 pointer
      if(typeof(nextPageSearch)!="S4"){
        #if not, stop scraping
        collect.page<-F
      } else {
        #otherwise move to the next page, pause for between 4 and 8 seconds, and then increment the page number
        tryCatch(
          {
            nextPageButton$clickElement()
          }, 
          error=function(e){
            rD[["server"]]$stop()
            return("Error")
          }
        )
        Sys.sleep(sample(seq(2,6),1))
        current.page<-current.page+1
      }
    }
  }
  rD[["server"]]$stop()
}