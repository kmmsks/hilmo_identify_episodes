
# Here we define ICD-10 diagnoses of interest. First dates of these diagnoses for each
# individual will be identified. Minimum age for each diagnostic group can be set.

# ICD-10 sub-chapter categories ----

dg_maingroups <-  c("f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9") %>% as.list()
names(dg_maingroups) <- dg_maingroups

# Diagnostic groups of interest ----
# For detecting first dates with diagnosis

dg_groups_of_interest <- append(dg_maingroups, list(
  f00_f03 = c("f00|f01|f02|f03"),
  f04_f09 = c("f04|f05|f06|f07|f09"),
  f20     = c("f20"),
  f2_no_sch = c("f21|f21|f22|f23|f24|f25|f26|f28|f29"),
  f30_f31   = c("f30|f31"),
  f32_f33   = c("f32|f33"),
  f3_other  = c("f34|f38|f39"),
  f40_f41   = c("f40|f41"),
  f42       = c("f42"),
  f4_other  = c("f43|f44|f45|f48"),
  f50   = c("f50"),
  f51   = c("f51"),
  f53   = c("f53"),
  f5_other = c("f54|f55|f59"),
  f90 = c("f90"),
  f9_other = c("f91|f92|f93|f94|f95|f98"),
  psychoses = c("f20|f22|f23|f24|f25|f28|f29|f301|f302|f308|f309|f311|f312|f315|f316|f323|f333|f1.5|f1.7"),
  imtm  = c("x6|x7|x80|x81|x82|x83|x84|x85")
)
)

# Minimum ages -----------------------------------------------------------------

# overall for everything except those defined in dg_specific

dg_min_ages <- list(
  overall = 5,
  dg_specific = 
    list(f7 = 1, f8 = 1, f9= 1, f90 = 1, f9_other = 1, f00_f03 = 35) %>% 
    stack() %>% setDT() %>%  setnames(c("ind", "values"), c("dg", "age")) %>% .[,.(dg, age)]
)

## Final fix:

dg_groups_w_min_ages <- dg_groups_of_interest %>%  stack() %>%   
  setDT() %>% setnames(c("ind", "values"), c("dg", "dg_code")) %>% 
  .[,age := 5]

dg_groups_w_min_ages[dg_min_ages$dg_specific, on = "dg", "age" := i.age]

rm(dg_groups_of_interest, dg_min_ages, dg_maingroups)



