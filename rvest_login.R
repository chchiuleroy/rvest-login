rm(list = ls());gc(reset = T);

library(magrittr)
library(rvest)
library(xml2)
library(XML)
library(xmlview)

pgsession <-html_session('http://61.66.117.198/WebService/RegisterService_hoa.asmx?op=getAllDivisionsbyHosp')
pgform    <-html_form(pgsession)[[1]]
filled_form <- set_values(pgform,
                          `hospaliasno` = 99,
                          `WSuserid` = "hoa_appmobile",
                          `WSPassword` = "appmobile_hoa")

result = submit_form(pgsession, filled_form)

case_home <- read_xml(result$response)
# first = xml_find_all(case_home, "//category")
test = as_list(case_home)
# test[[3]][[2]]
# attr(test[[3]][[2]], "id")
# length(test[[3]])

qq = function(test, i) {
  z = test[[i]]
  l = length(z)
  d = matrix(, l, 3)
  for(j in 2:l) {
    x = unlist(test[[i]][[1]])
    y0 = test[[i]][[j]]
    y1 = unlist(y0)
    y2 = attr(y0, "id")
    d[j, ] = c(x, y1, y2)
  }
  
  #  d = data.frame(category = x, division = y1, disision.id = y2, row.names = NULL)
  return(na.omit(d))
  
}

temp = rbind(qq(test, 3), qq(test, 4), qq(test, 5), qq(test, 6), qq(test, 7))
colnames(temp) = c("category", "division", "division.id")

# xml_find_all(case_home, "//division")
# xml_find_all(case_home, "//name")
# xml_find_all(case_home, "//name//division")

# category = c(rep("ず╰", xml_length(first[[1]])-1), rep("╰", xml_length(first[[2]])-1), rep("包ㄠ╰", xml_length(first[[3]])-1), 
#              rep("い洛╰", xml_length(first[[4]])-1), rep("疭盡", xml_length(first[[5]])-1))
# division_name = xml_text(xml_find_all(case_home, "//division//name"))
# division_id = xml_text(xml_find_all(case_home, "//@id"))
# 
# temp = data.frame(cbind(category, division_id, division_name))


# ---------------------------------- #

#depno_id = (unique(temp$division.id))

hospaliasno = 99
WSuserid = "hoa_appmobile"
WSPassword = "appmobile_hoa"

pgsession <-html_session("http://61.66.117.198/WebService/RegisterService_hoa.asmx?op=getAllDocInfobyHosp")
pgform    <-html_form(pgsession)[[1]]
filled_form <- set_values(pgform,
                          `hospaliasno` = hospaliasno,
                          `deptno` = "",
                          `WSuserid` = WSuserid,
                          `WSPassword` = WSPassword)

result = submit_form(pgsession, filled_form)

case_home <- read_xml(result$response)
test = as_list(case_home)

qa = function(i) {
  x = test$doctorList[[i]]
  y1 = unlist(test$doctorList[[i]])
  y2 = attr(test$doctorList[[i]], "id")
  y3 = t(sapply(2:length(x), function(z){attr(x[[z]], "id")}))
  y = c(y1, y2, y3)
  return(y)
}

u = sapply(1:length(test$doctorList), qa)

v = matrix(, length(test$doctorList), 10)

for(i in 1:length(test$doctorList)) {
  v[i, ] = c(qa(i), rep(NA, 10 -length(qa(i))))
}

url1 = "http://61.66.117.198/WebService/RegisterService_hoa.asmx?op=getScheduleByDivisionbyHosp"

hospaliasno = 99
WSuserid = "hoa_appmobile"
WSPassword = "appmobile_hoa"

pgsession <-html_session(url1)
pgform    <-html_form(pgsession)[[1]]
filled_form <- set_values(pgform,
                          `hospaliasno` = hospaliasno,
                          `divisionId`  = "",
                          `fromDate`    = "2017-09-04",
                          `toDate`      = "2017-09-05",
                          `WSuserid`    = WSuserid,
                          `WSPassword`  = WSPassword)

result = submit_form(pgsession, filled_form)

case_home <- read_xml(result$response)
xml_view(case_home)
test = as_list(case_home)

qb = function(i, j) {
  
  date = attr(test$scheduleList[[i]], "date") # date
  x = test$scheduleList[[i]][[j]] # i: date, j: period 
  l = length(x) # k
  x0 = attr(x, "id")# period.id 1:Ν 2:と 3:禘
  
  m = matrix(, l, 5)
  
  for(k in 1: l) {
    y0 = test$scheduleList[[i]][[j]][[k]]
    y1 = unlist(y0) # people ヘ玡本计
    y2 = attr(y0, "id") # NO.禘
    y3 = attr(y0, "status") # clinic status 1本腹-2ヰ禘-4筄999肂骸Τ1本腹
    m[k, ] = c(y1, y2, y3)
  }
  
  
  d = cbind(rep(date, l), rep(x0, l), m) 
  colnames(d) = c("ら戳", "禘琿", "禘", "ヘ玡本腹计", "洛ネ", "禘ID", "禘篈")
  return(d)
  
}