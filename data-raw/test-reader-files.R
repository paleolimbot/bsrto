
bsrto_ftp <- "ftp://dfoftp.ocean.dal.ca/pub/dfo/barrow/"

curl::curl_download(
  "ftp://dfoftp.ocean.dal.ca/pub/dfo/barrow/1998/ctd/processed/CTD_98911_10P_11_DN.ODF",
  "inst/ex/CTD_98911_10P_11_DN.ODF"
)
