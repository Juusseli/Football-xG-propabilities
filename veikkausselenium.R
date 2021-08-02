library(RSelenium)
library(tidyverse)
library(purrr)
library(png)
library(RCurl)
driver <- rsDriver(browser=c("chrome"))
remote_driver <- driver[["client"]]
remote_driver$open()


baseurl <- 'http://mc.instatscout.com/veikkausliiga/'
remote_driver$setWindowSize(800L, 650L)
#avataan esimm?inen sivu ja klikataan ensimm?isen joukkueen laukaukset esille
gameid<-1682117
df <- data.frame()
koordinaatit <-data.frame()
kotidf <- data.frame()
vierasdf <- data.frame()
for(gameid in 1682143:1682143){
remote_driver$navigate(paste0(baseurl,gameid))
Sys.sleep(7)
address_element<-remote_driver$findElement(using = 'class', value = 'team-stats-tabs-list-item')
Sys.sleep(2)
address_element$clickElement()
Sys.sleep(2)
#etsit??n kohti mennneet laukaukset

#df <- data.frame()

#looppi kohti laukaus-tooltippien l?pi ja tallennus dataframeen
shot_type<-remote_driver$findElements(using ='css selector',value = 'a.shot.episode')
Sys.sleep(2)
for(ele in seq_along(shot_type)){
  tyyppi <-shot_type[[ele]]
  tyyppi$mouseMoveToLocation(webElement = tyyppi)
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  ele<-remote_driver$findElements(using = 'css selector', value='div.tooltip-time')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  sot<-remote_driver$findElements(using = 'css selector', value='div.tooltip-shoot-type')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  texts_sot <- unlist(lapply(sot, function(x) x$getElementText()))
  texts_sot <- texts_sot[texts_sot != ""]
  texts <- unlist(lapply(ele, function(x) x$getElementText()))
  texts <- texts[texts != ""]
  texts <- gsub(',', '', texts)
  texts_sot <- gsub(' ', '.', texts_sot)
  texts <-paste(texts, sep = ' ', collapse = ' ')
  texts <- do.call(c, list(texts, texts_sot, gameid))
  texts <-paste(texts, sep = ' ', collapse = ' ')
  df=rbind(df,list(texts))
  Sys.sleep(0.5)
}
remote_driver$setTimeout(type ='page load', milliseconds =10000)



#looppi maali-tooltippien l?pi ja tallennus dataframeen
maali_element<-remote_driver$findElements(using = 'css selector', value = 'a.goal.episode')
Sys.sleep(2)
for(muali in seq_along(maali_element)){
  maalit <-maali_element[[muali]]
  maalit$mouseMoveToLocation(webElement = maalit)
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  muali<-remote_driver$findElements(using = 'css selector', value='div.tooltip-time')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  iag<-remote_driver$findElements(using = 'css selector', value='div.tooltip-shoot-type')
  text_iag <- unlist(lapply(iag, function(x) x$getElementText()))
  text_iag <- text_iag[text_iag != ""]
  texts_m <- unlist(lapply(muali, function(x) x$getElementText()))
  texts_m <- texts_m[texts_m != ""]
  texts_m <- gsub(',', '', texts_m)
  text_iag <- gsub(' ', '.', text_iag)
  texts_m <-paste(texts_m, sep = ' ', collapse = ' ')
  texts_m <- do.call(c, list(texts_m, text_iag, gameid))
  texts_m <-paste(texts_m, sep = ' ', collapse = ' ')
  df=rbind(df,list(texts_m))
  Sys.sleep(0.5)
}
remote_driver$setTimeout(type ='page load', milliseconds =10000)

#looppi ohilaukaus-tooltippien l?pi ja tallennus dataframeen
ohi_element<-remote_driver$findElements(using = 'css selector', value = 'a.miss.episode')
Sys.sleep(2)
for(ohi in seq_along(ohi_element)){
  ohitukset <-ohi_element[[ohi]]
  ohitukset$mouseMoveToLocation(webElement = ohitukset)
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  ohi<-remote_driver$findElements(using = 'css selector', value='div.tooltip-time')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  ohil<-remote_driver$findElements(using = 'css selector', value='div.tooltip-shoot-type')
  text_ohi <- unlist(lapply(ohil, function(x) x$getElementText()))
  text_ohi <- text_ohi[text_ohi != ""]
  texts_o <- unlist(lapply(ohi, function(x) x$getElementText()))
  texts_o <- texts_o[texts_o != ""]
  texts_o <- gsub(',', '', texts_o)
  text_ohi <- gsub(' ', '.', text_ohi)
  texts_o <-paste(texts_o, sep = ' ', collapse = ' ')
  texts_o <- do.call(c, list(texts_o, text_ohi, gameid))
  texts_o <-paste(texts_o, sep = ' ', collapse = ' ')
  df=rbind(df,list(texts_o))
  Sys.sleep(0.5)
}
remote_driver$setTimeout(type ='page load', milliseconds =10000)




#hyp?t??n  vierasjoukkueen laukaisukarttaan
Sys.sleep(1)
address_element<-remote_driver$findElement(using = 'css selector', value = 'div.ui-select')
address_element$clickElement()
Sys.sleep(1)
address_element<-remote_driver$findElement(using = 'xpath', value = '//*[@id="MatchCenter"]/div/div[4]/div[1]/div/div[1]/div[2]/div[2]/div/div/ul/li[2]/a')
Sys.sleep(1)
address_element$clickElement()
Sys.sleep(1)

#tyhjennet??n kotijoukkueen muuttujien sis?lt?
rm(shot_type)
rm(maali_element)
rm(ohi_element)
rm(ele)
rm(tyyppi)
rm(texts)
rm(muali)
rm(maalit)
rm(texts_m)
rm(ohi)
rm(ohitukset)
rm(texts_o)
rm(ohil)
rm(text_ohi)
rm(text_iag)
rm(iag)
rm(texts_sot)
rm(sot)


shot_type<-remote_driver$findElements(using ='css selector',value = 'a.shot.episode')

Sys.sleep(2)
for(ele in seq_along(shot_type)){
  tyyppi <-shot_type[[ele]]
  tyyppi$mouseMoveToLocation(webElement = tyyppi)
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  ele<-remote_driver$findElements(using = 'css selector', value='div.tooltip-time')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  sot<-remote_driver$findElements(using = 'css selector', value='div.tooltip-shoot-type')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  texts_sot <- unlist(lapply(sot, function(x) x$getElementText()))
  texts_sot <- texts_sot[texts_sot != ""]
  texts <- unlist(lapply(ele, function(x) x$getElementText()))
  texts <- texts[texts != ""]
  texts <- gsub(',', '', texts)
  texts_sot <- gsub(' ', '.', texts_sot)
  texts <-paste(texts, sep = ' ', collapse = ' ')
  texts <- do.call(c, list(texts, texts_sot, gameid))
  texts <-paste(texts, sep = ' ', collapse = ' ')
  df=rbind(df,list(texts))
  Sys.sleep(0.5)
}
remote_driver$setTimeout(type ='page load', milliseconds =10000)



#looppi maali-tooltippien l?pi ja tallennus dataframeen
maali_element<-remote_driver$findElements(using = 'css selector', value = 'a.goal.episode')
Sys.sleep(2)
for(muali in seq_along(maali_element)){
  maalit <-maali_element[[muali]]
  maalit$mouseMoveToLocation(webElement = maalit)
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  muali<-remote_driver$findElements(using = 'css selector', value='div.tooltip-time')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  iag<-remote_driver$findElements(using = 'css selector', value='div.tooltip-shoot-type')
  text_iag <- unlist(lapply(iag, function(x) x$getElementText()))
  text_iag <- text_iag[text_iag != ""]
  texts_m <- unlist(lapply(muali, function(x) x$getElementText()))
  texts_m <- texts_m[texts_m != ""]
  texts_m <- gsub(',', '', texts_m)
  text_iag <- gsub(' ', '.', text_iag)
  texts_m <-paste(texts_m, sep = ' ', collapse = ' ')
  texts_m <- do.call(c, list(texts_m, text_iag, gameid))
  texts_m <-paste(texts_m, sep = ' ', collapse = ' ')
  df=rbind(df,list(texts_m))
  Sys.sleep(0.5)
}
remote_driver$setTimeout(type ='page load', milliseconds =10000)

#looppi ohilaukaus-tooltippien l?pi ja tallennus dataframeen
ohi_element<-remote_driver$findElements(using = 'css selector', value = 'a.miss.episode')
Sys.sleep(2)
for(ohi in seq_along(ohi_element)){
  ohitukset <-ohi_element[[ohi]]
  ohitukset$mouseMoveToLocation(webElement = ohitukset)
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  ohi<-remote_driver$findElements(using = 'css selector', value='div.tooltip-time')
  remote_driver$setTimeout(type ='page load', milliseconds =10000)
  ohil<-remote_driver$findElements(using = 'css selector', value='div.tooltip-shoot-type')
  text_ohi <- unlist(lapply(ohil, function(x) x$getElementText()))
  text_ohi <- text_ohi[text_ohi != ""]
  texts_o <- unlist(lapply(ohi, function(x) x$getElementText()))
  texts_o <- texts_o[texts_o != ""]
  texts_o <- gsub(',', '', texts_o)
  text_ohi <- gsub(' ', '.', text_ohi)
  texts_o <-paste(texts_o, sep = ' ', collapse = ' ')
  texts_o <- do.call(c, list(texts_o, text_ohi, gameid))
  texts_o <-paste(texts_o, sep = ' ', collapse = ' ')
  df=rbind(df,list(texts_o))
  Sys.sleep(0.5)
}
remote_driver$setTimeout(type ='page load', milliseconds =10000)



colnames(df)<-'terve'



remote_driver$navigate(paste0(baseurl,gameid))
Sys.sleep(7)
remote_driver$setTimeout(type ='page load', milliseconds =10000)
address_element<-remote_driver$findElement(using = 'class', value = 'team-stats-tabs-list-item')
Sys.sleep(3)
address_element$clickElement()
Sys.sleep(2)

joukkue_koti<-remote_driver$findElement(using ='xpath',value = '/html/body/div/div/div[3]/div/h1/span[1]/a')
kotitiedosto<-joukkue_koti$getElementText()
kotitiedostotext<-kotitiedosto[[1]]
kotitiedostotext<-gsub('"',"",kotitiedostotext)

#haetaan koordinaatit
shot_type<-remote_driver$findElements(using ='css selector',value = 'a.shot.episode')
Sys.sleep(1)
shot_kohti<-unlist(lapply(shot_type, function(x) x$getElementAttribute('style')))
shot_kohti <- gsub('left: ', '', shot_kohti)
shot_kohti <- gsub('%; top:', '', shot_kohti)
shot_kohti <- gsub('%;', '', shot_kohti)
dfkohti<-data.frame(shot_kohti)
dfkohti<-data.frame(do.call('rbind',strsplit(as.character(dfkohti$shot_kohti),' ',fixed=TRUE)))
dfkohti<-data.frame(dfkohti,kotitiedostotext)


maali_element<-remote_driver$findElements(using = 'css selector', value = 'a.goal.episode')
Sys.sleep(1)
shot_maali<-unlist(lapply(maali_element, function(x) x$getElementAttribute('style')))
shot_maali <- gsub('left: ', '', shot_maali)
shot_maali <- gsub('%; top:', '', shot_maali)
shot_maali <- gsub('%;', '', shot_maali)
dfmaali<-data.frame(shot_maali)
dfmaali<-data.frame(do.call('rbind',strsplit(as.character(dfmaali$shot_maali),' ',fixed=TRUE)))
dfmaali<-data.frame(dfmaali,kotitiedostotext)

ohi_element<-remote_driver$findElements(using = 'css selector', value = 'a.miss.episode')
Sys.sleep(1)
shot_ohi<-unlist(lapply(ohi_element, function(x) x$getElementAttribute('style')))
shot_ohi <- gsub('left: ', '', shot_ohi)
shot_ohi <- gsub('%; top:', '', shot_ohi)
shot_ohi <- gsub('%;', '', shot_ohi)
dfohi<-data.frame(shot_ohi)
dfohi<-data.frame(do.call('rbind',strsplit(as.character(dfohi$shot_ohi),' ',fixed=TRUE)))
dfohi<-data.frame(dfohi,kotitiedostotext)

kotidf<-as.data.frame(do.call('rbind',list(dfkohti,dfmaali,dfohi)))



address_element<-remote_driver$findElement(using = 'css selector', value = 'div.ui-select')
Sys.sleep(1)
address_element$clickElement()
Sys.sleep(2)
address_element<-remote_driver$findElement(using = 'xpath', value = '//*[@id="MatchCenter"]/div/div[4]/div[1]/div/div[1]/div[2]/div[2]/div/div/ul/li[2]/a')
Sys.sleep(2)
address_element$clickElement()
Sys.sleep(2)

rm(shot_type)
rm(maali_element)
rm(ohi_element)
rm(ele)
rm(tyyppi)
rm(texts)
rm(muali)
rm(maalit)
rm(texts_m)
rm(ohi)
rm(ohitukset)
rm(texts_o)
rm(ohil)
rm(text_ohi)
rm(text_iag)
rm(iag)
rm(texts_sot)
rm(sot)
rm(shot_kohti)
rm(shot_maali)
rm(shot_ohi)

joukkue_vieras<-remote_driver$findElement(using ='xpath',value = '/html/body/div/div/div[3]/div/h1/span[3]/a')
vierastiedosto<-joukkue_vieras$getElementText()
vierastiedostotext<-vierastiedosto[[1]]
vierastiedostotext<-gsub('"',"",vierastiedostotext)



#haetaan koordinaatit
shot_type<-remote_driver$findElements(using ='css selector',value = 'a.shot.episode')
Sys.sleep(1)
shot_kohti<-unlist(lapply(shot_type, function(x) x$getElementAttribute('style')))
shot_kohti <- gsub('left: ', '', shot_kohti)
shot_kohti <- gsub('%; top:', '', shot_kohti)
shot_kohti <- gsub('%;', '', shot_kohti)
dfkohti<-data.frame(shot_kohti)
dfkohti<-data.frame(do.call('rbind',strsplit(as.character(dfkohti$shot_kohti),' ',fixed=TRUE)))
dfkohti<-data.frame(dfkohti,vierastiedostotext)


maali_element<-remote_driver$findElements(using = 'css selector', value = 'a.goal.episode')
Sys.sleep(1)
shot_maali<-unlist(lapply(maali_element, function(x) x$getElementAttribute('style')))
shot_maali <- gsub('left: ', '', shot_maali)
shot_maali <- gsub('%; top:', '', shot_maali)
shot_maali <- gsub('%;', '', shot_maali)
dfmaali<-data.frame(shot_maali)
dfmaali<-data.frame(do.call('rbind',strsplit(as.character(dfmaali$shot_maali),' ',fixed=TRUE)))
dfmaali<-data.frame(dfmaali,vierastiedostotext)

ohi_element<-remote_driver$findElements(using = 'css selector', value = 'a.miss.episode')
Sys.sleep(1)
shot_ohi<-unlist(lapply(ohi_element, function(x) x$getElementAttribute('style')))
shot_ohi <- gsub('left: ', '', shot_ohi)
shot_ohi <- gsub('%; top:', '', shot_ohi)
shot_ohi <- gsub('%;', '', shot_ohi)
dfohi<-data.frame(shot_ohi)
dfohi<-data.frame(do.call('rbind',strsplit(as.character(dfohi$shot_ohi),' ',fixed=TRUE)))
dfohi<-data.frame(dfohi,vierastiedostotext)

vierasdf<-as.data.frame(do.call('rbind',list(dfkohti,dfmaali,dfohi)))

#write.table(dfohi, file='dfohi.csv',append = TRUE,quote = FALSE, dec = ',', col.names = TRUE,sep=',')


kotijoukkue_element<-remote_driver$findElement(using = 'xpath', value = '/html/body/div/div/div[4]/div[1]/div/div[2]/div[1]/div/div[3]/div[1]/img')
koti<-kotijoukkue_element$getElementAttribute('src')
kotifile <-koti[[1]]
kotikuva<-readPNG(getURLContent(kotifile),native = TRUE)


vierasjoukkue_element<-remote_driver$findElement(using = 'xpath', value = '/html/body/div/div/div[4]/div[1]/div/div[2]/div[1]/div/div[3]/div[3]/img')
vieras<-vierasjoukkue_element$getElementAttribute('src')
vierasfile <-vieras[[1]]
vieraskuva<-readPNG(getURLContent(vierasfile),native = TRUE)

dfu<-data.frame(do.call('rbind',strsplit(as.character(df$terve),' ',fixed=TRUE)))


colnames(kotidf)<-c('x','y','joukkue')
colnames(vierasdf)<-c('x','y','joukkue')

koordinaatit <- do.call('rbind',list(kotidf, vierasdf))
daatta<- do.call('cbind', list(dfu, koordinaatit))
colnames(daatta) <-c('home/away','puoliaika','paika','aika','pvm','laukaisu','gameid','x','y','joukkue')
tilastot<-daatta
rm(koordinaatit)
#colnames(koordinaatit) <-c('x','y')
#write.table(koordinaatit, file='pelikoord.csv',append = TRUE,quote = FALSE, dec = ',', col.names = TRUE,sep=';')
# newdf<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list, accumulate=FALSE)

#hei<-do.call("rbind", list(dfkohti,dfmaali)) 
#heit<-do.call("rbind", list(hei,dfohi)) 


#dfuu <- do.call("cbind", list(dfu, dfkoord))

#colnames(dfu) <-c('numero','home/away','puoliaika','paika','aika','pvm','laukaisu','gameid')
#write.table(dfu, file='pelitiedot.csv',append = TRUE,quote = FALSE, dec = ',', col.names = TRUE,sep=',')
rm(dfu)

df <- data.frame()

}

paiva<-remote_driver$findElement(using ='class',value = 'match-header__date')
pvm <- paiva$getElementText()
#pvm<-unlist(pvm)

tiedostot<-read.csv('pelitiedot.csv', sep = ',',col.names = FALSE)
koord = read.table('pelikoord.csv',header=TRUE,dec = ',', sep = ';',col.names = TRUE)

jeeejee <- do.call('cbind',list(tiedostot, koord))

jeeejee <- data.frame(lapply(jeeejee, as.character), stringsAsFactors=FALSE)
colnames(daatta) <-c('home/away','puoliaika','paika','aika','pvm','laukaisu','gameid')
tilastot<-daatta
write.table(jeeejee, file='pelitilastot.csv',append = TRUE,quote = FALSE, dec = ',', col.names = TRUE,sep=',')

tilastot<-read.csv('pelitilastot.csv', sep = ',', quote ='')


peli_setti<-remote_driver$findElements(using ='class',value = 't-compare')
peli <- unlist(lapply(peli_setti, function(x) x$getElementText()))
view(peli)
print(peli)
peli
