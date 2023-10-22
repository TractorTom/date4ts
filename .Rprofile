source("renv/activate.R")

# options(download.file.method = "curl")
options(download.file.extra = "--noproxy \"*\"")

# options(download.file.method = "libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD = "libcurl")


# if (Sys.getenv("USERDNSDOMAIN") == "AD.INSEE.INTRA") {
#     # Cas sur un ordi de l'Insee (AUS ou sessino classique)
#     options(repos = c(nexuspublic = "https://nexus.insee.fr/repository/r-public"))
# } else {
#     # Autres cas
#     options(repos = c(CRAN = "https://cloud.r-project.org"))
# }
# renv::snapshot()


options(repos = c(CRAN = "https://cloud.r-project.org",
                  nexuspublic = "https://nexus.insee.fr/repository/r-public"))

