
spaces <- tibble::tribble(
  ~Space, ~Indicator, ~Description,
  "Electoral",     "v2x_veracc_osp", "Vertical accountability index",
  "Associational", "v2xcs_ccsi", "",
  "Individual",    "v2xcl_rol", "Rule of law index",
  "Informational", "v2x_freexp_altinf", "",
  "Governing",     "v2x_horacc_osp", "",
  "Economic",      "v2x_pubcorr", "Absence of public corruption. Note that this is inverted from the original V-Dem variable so that high values indicate absence of public corruption."
)

usethis::use_data(spaces, overwrite = TRUE)
