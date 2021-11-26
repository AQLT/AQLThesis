#' NBER Business US Cycle Reference Dates
#'
#' National Bureau of Economic Research’s Business Cycle Dating Committee maintains a chronology of US business cycles. The chronology identifies the dates of peaks and troughs that frame economic recessions and expansions. A recession is the period between a peak of economic activity and its subsequent trough, or lowest point. Between trough and peak, the economy is in an expansion. Expansion is the normal state of the economy; most recessions are brief. However, the time that it takes for the economy to return to its previous peak level of activity or its previous trend path may be quite extended. According to the NBER chronology, the most recent peak occurred in February 2020. The most recent trough occurred in April 2020.
#' @source \url{https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions}
#' @name nber_tp
#' @rdname nber_tp
"nber_tp_m"
#' @rdname nber_tp
"nber_tp_q"

#
# data = structure(list(V1 = c("", "June 1857 (1857Q2)", "October 1860 (1860Q3)",
#                              "April 1865 (1865Q1)", "June 1869 (1869Q2)", "October 1873 (1873Q3)",
#                              "March 1882 (1882Q1)", "March 1887 (1887Q2)", "July 1890 (1890Q3)",
#                              "January 1893 (1893Q1)", "December 1895 (1895Q4)", "June 1899 (1899Q3)",
#                              "September 1902 (1902Q4)", "May 1907 (1907Q2)", "January 1910 (1910Q1)",
#                              "January 1913 (1913Q1)", "August 1918 (1918Q3)", "January 1920 (1920Q1)",
#                              "May 1923 (1923Q2)", "October 1926 (1926Q3)", "August 1929 (1929Q3)",
#                              "May 1937 (1937Q2)", "February 1945 (1945Q1)", "November 1948 (1948Q4)",
#                              "July 1953 (1953Q2)", "August 1957 (1957Q3)", "April 1960 (1960Q2)",
#                              "December 1969 (1969Q4)", "November 1973 (1973Q4)", "January 1980 (1980Q1)",
#                              "July 1981 (1981Q3)", "July 1990 (1990Q3)", "March 2001 (2001Q1)",
#                              "December 2007 (2007Q4)", "February 2020 (2019Q4)"), V2 = c("December 1854 (1854Q4)",
#                                                                                          "December 1858 (1858Q4)", "June 1861 (1861Q3)", "December 1867 (1868Q1)",
#  "December 1870 (1870Q4)", "March 1879 (1879Q1)", "May 1885 (1885Q2)",
#                                                                                          "April 1888 (1888Q1)", "May 1891   ", "June 1894 (1894Q2)",
#                                                                                          "June 1897 (1897Q2)", "December 1900 (1900Q4)", "August 1904 (1904Q3)",
#                                                                                          "June 1908 (1908Q2)", "January 1912 (1911Q4)", "December 1914 (1914Q4)",
#                                                                                          "March 1919 (1919Q1)", "July 1921 (1921Q3)", "July 1924 (1924Q3)",
#                                                                                          "November 1927 (1927Q4)", "March 1933 (1933Q1)", "June 1938 (1938Q2)",
#                                                                                          "October 1945 (1945Q4)", "October 1949 (1949Q4)", "May 1954 (1954Q2)",
#                                                                                          "April 1958 (1958Q2)", "February 1961 (1961Q1)", "November 1970 (1970Q4)",
#                                                                                          "March 1975 (1975Q1)", "July 1980 (1980Q3)", "November 1982 (1982Q4)",
#                                                                                          "March 1991 (1991Q1)", "November 2001 (2001Q4)", "June 2009 (2009Q2)",
#                                                                                          "April 2020 (2020Q2)")), class = "data.frame", row.names = c(NA,
#                                                                                                                                                       -35L))
# months_all <- c("January" = 0, "February" = 1, "March" = 2, "April" = 3,
#                 "May" = 4, "June" = 5, "July" = 6, "August" = 7, "September"= 8, "October" = 9, "November" = 10,
#                 "December" = 11)
# extract_monthly <- function(x){
#   months <- sapply(strsplit(x,"\\("), `[`,1)
#   months <- gsub("\\s*$", "", months)
#
#   sapply(strsplit(months, "\\s"), function(x){
#     as.numeric(x[2]) + months_all[x[1]] / 12
#   })
# }
# extract_quaterly <- function(x){
#   quarter <- sapply(strsplit(x,"\\("), `[`,2)
#   quarter <- gsub("\\)", "", quarter)
#
#   sapply(strsplit(quarter, "Q"), function(x){
#     as.numeric(x[1]) + as.numeric(x[2]) / 4
#   })
# }
#
# nber_tp_m <- apply(data, 2, extract_monthly)
# nber_tp_q <- apply(data, 2, extract_quaterly)
# colnames(nber_tp_m) <-
#   colnames(nber_tp_q) <- c("Peak", "Trough")
# usethis::use_data(nber_tp_m, nber_tp_q)
