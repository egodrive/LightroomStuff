ExportFromLR = function(DIR, 
                        LR_FILE,
                        Nynorsk = FALSE,
                        Nøyaktighet = 5,
                        FørsteDato = "01-01-1970"){
  library(RSQLite)
  library(data.table)
  library(zoo)
  # Nøyaktighet: GPS nøyaktighet. Basert på erfaring med kamera
  # DIR: Hvor skal filer lagres?
  # LR_FILE: lokalitet hvor man skal finne LR-katalogen
  # Nynorsk: har man brukt nynorske namn på arter?
  # FromDate: fra dato man skal jobbe med. I tilfelle man bare vil hente ut data etter en viss dato.
  # DIR = "C:/Users/post/Dropbox/"
  # LR_FILE = "C:/Users/post/Dropbox/Bildekatalog_ForLR_vers8.lrcat"
  setwd(DIR)
  # Laster inn skjema som viser hvordan filer skal være strukturert
  # This currently fails to download a proper copy of the file, the file get corrupted, so currently needs to be manually downloaded using the same link in e.g. Chrome, Internet Explorer
  if(!file.exists("SightingTemplate3.xlsx")){download.file(url = "https://www.artsobservasjoner.no/Download/SightingTemplate", destfile = "SightingTemplate3.xlsx", method = "curl")}
  
  # Laster inn fil med artsnavn fra Artsdatabanken
  if(!file.exists("Artsnavnebase.csv")){download.file(url = "http://eksport.artsdatabanken.no/Artsnavnebase/Artsnavnebase.csv", destfile = "Artsnavnebase.csv", method = "curl")}
  Reference = fread("Artsnavnebase.csv", na.strings = "")
  # There is an error with the first colum
  names(Reference) = c(names(Reference)[-1], "NA")
  # Strukturer slik at radene bare inkluderer nivå lavere enn familie
  Reference = Reference[,c("Rike", "Rekke", "Klasse", "Orden", "Familie", "Slekt", "Art", "Underart", "PopulærnavnBokmål", "PopulærnavnNynorsk"),with = F]
  
  #Reference2 = Reference[1:3]
  Reference[,Nivå:=apply(Reference, 1, function(ii){
    names(Reference)[min(which(is.na(ii)))-1]
  })]
  
  Reference[,PopulærnavnBokmål:=tolower(PopulærnavnBokmål)]
  Reference[,LatinskNavn := ifelse(Nivå=="Underart",tolower(paste(Slekt, Art, Underart)),ifelse(Nivå=="Art",tolower(paste(Slekt, Art)),NA))]
  
  # Are any of the keyworded images in the Reference file columns
  # PopulærnavnBokmål
  # LatinskNavn
  # PopulærnavnNynorsk
  # ? 
  
  db <- dbConnect(RSQLite::SQLite(), 
                  dbname=LR_FILE)
  
  KeywordsUsed = data.table(dbReadTable(db,"AgLibraryKeywordPopularity")) # Keywords in use
  KeywordsUsed = KeywordsUsed[popularity>0]
  KeywordsUsed
  
  
  dK =  data.table(dbReadTable(db,"AgLibraryKeyword")) # the hierarchy of keywords
  dK = dK[id_local %in% KeywordsUsed$tag] # Subset to those used
  
  # LR keywords existing in the reference list
  dK$lc_name[grepl("kløver",dK$lc_name, ignore.case = T)]
  dK$lc_name[419]
  idx1 = (sapply(dK$lc_name, function(xx) xx %in% Reference$LatinskNavn))
  
  if(Nynorsk){
    Reference[,PopulærnavnNynorsk:=tolower(PopulærnavnNynorsk)]
    idx2 = (sapply(dK$name, function(xx) xx  %in% Reference$PopulærnavnNynorsk))
  }else{
    idx2 = (sapply(dK$lc_name, function(xx) xx  %in% Reference$PopulærnavnBokmål))
  }
  
  # Subset to keyword tags included in the reference list, aka the taxonomy
  dK = dK[apply(cbind(idx1,idx2),1,max)==1]
  dK = dK[,c("id_local", "id_global", "lc_name", "name")]
  setnames(dK, "id_local", "tag")
  
  ## Which images have these keywords?
  dI =  data.table(dbReadTable(db,"AgLibraryKeywordImage")) # the hierarchy of keywords
  dI = dI[tag %in% dK$tag]
  
  dI = dK[dI, on = "tag"][,id_local:=NULL]
  
  # Metadata
  dM_ =  data.table(dbReadTable(db,"Adobe_images"))
  dL =  data.table(dbReadTable(db,"AgLibraryFile"))
  
  dM_ = dM_[,c("id_local", "captureTime")]
  dM_[,captureTime:=sapply(strsplit(captureTime,"T"), function(x) x[2])]
  setnames(dM_, "id_local", "image")
  
  dM =  data.table(dbReadTable(db,"AgHarvestedExifMetadata"))
  dM = dM[hasGPS==1]
  dM = dM[image %in% dI$image]
  
  # Collate all
  datas = dM
  datas[,startdato:=paste(dateDay, dateMonth, dateYear, sep = ".")][,TilDato:=startdato]  
  # Subset to relevant data range
  datas = datas[as.Date(startdato, format = "%d.%m.%Y")>as.Date(FørsteDato, format = "%d-%m-%Y")]
  
  datas[,Øst:=gpsLongitude][,Nord:=gpsLatitude]
  datas = datas[,c("id_local", "image", "startdato", "TilDato", "Øst", "Nord")]
  datas = dM_[datas, on = "image"]
  setnames(datas, "captureTime", "Fra klokkeslett")
  
  datas = datas[dI, on = "image"]
  # Bare stedfestede data
  datas = datas[!is.na(Øst)]
  # replikate, ingen interesse av flere rapporter av samme individid. velge en arbitrær avkutt på 1 time mellom observasjoner
  setorder(datas, name, startdato)
  datas[,`Fra klokkeslett`:=as.ITime(datas$`Fra klokkeslett`, format = "%H:%M:%S")]
  datas[,Hour:=hour(`Fra klokkeslett`)]
  datas = datas[,.SD[1], c("name", "startdato", "Hour")]
  
  datas[,mins := round(minute(datas$`Fra klokkeslett`) + second(datas$`Fra klokkeslett`)/60)]
  datas[,mins := ifelse(mins==60,59,mins)]
  
  datas[,`Fra klokkeslett`:=paste(Hour,mins, sep = ":")][,c("Hour", "mins"):=NULL]
  
  
  # Bare bruk bilder med nivå
  idx = sapply(datas$lc_name, 
               function(ii){
                 a = ii %in% Reference[Nivå %in% c("Art", "Slekt", "Familie")]$PopulærnavnBokmål
                 a2 = ii %in% Reference[Nivå %in% c("Art", "Slekt", "Familie")]$LatinskNavn
                 a3 = ii %in% Reference[Nivå %in% c("Art", "Slekt", "Familie")]$PopulærnavnNynorsk
                 max(c(a,a2,a3))
               })
  
  datas = datas[idx==1]
  setnames(datas, "name", "artsnavn")
  
  
  # Legg til en info-kolonne om denne prosedyren
  datas[,`Privat kommentar (kun synlig for deg selv)`:="Automatisk generert fra Lightroom info mha script"]
  datas[,Nøyaktighet := Nøyaktighet] # Basert på når kameraet ligger i ro på samme plass
  
  Locs = data.table(dbReadTable(db,"AgHarvestedIptcMetadata")) # 
  Locs[,c("image", "cityRef")]
  Locs_ = data.table(dbReadTable(db,"AgInternedIptcCity")) # 
  Locs_ = Locs_[,c("id_local", "value")]
  setnames(Locs_, c("id_local","value"), c("image","lokalitetsnavn"))
  Locs = Locs_[Locs, on = "image"]
  Locs[,c("image", "lokalitetsnavn")]
  # Hvis data mangler bruke 'Norge' som default
  datas[, lokalitetsnavn:=ifelse(is.na(lokalitetsnavn), "Norge",lokalitetsnavn)]
  
  # Fjern overflødige kolonner
  datas[,c("id_global", "Hour", "image", "id_local", "tag", "lc_name"):=NULL]
  
  # Har fortsatt ikke funnet ut hvordan jeg får linket med fil-navn, så fjerner denne for nå
  #datas = dL[,c("id_global", "idx_filename")][datas, on = "id_global"]
  #datas[,idx_filename:=NULL]
  setnames(datas, c("startdato","TilDato"), c("Fra dato","Til dato"))
  datas = datas[,c("artsnavn", "lokalitetsnavn","Øst", "Nord", "Nøyaktighet", "Fra dato", "Til dato", "Fra klokkeslett", "Privat kommentar (kun synlig for deg selv)")]
  # Artsobservasjoner tillater å lime inn fra Excel-ark
  # dette tilsvarer å klipp/kopier-lim fra en tab-delt tekstfil
  write.table(datas, file = "FraLR_TilArtsobservasjoner.txt", col.names = T,
              quote = F, row.names = F, fileEncoding = "UTF-8", sep = "\t")
}
ExportFromLR()
