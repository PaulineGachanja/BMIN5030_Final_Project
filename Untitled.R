
library(ieugwasr)


OPENGWAS_JWT="eyJhbGciOiJSUzI1NiIsImtpZCI6ImFwaS1qd3QiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhcGkub3Blbmd3YXMuaW8iLCJhdWQiOiJhcGkub3Blbmd3YXMuaW8iLCJzdWIiOiJ3LnBhdWxpbmVnYWNoYW5qYUBnbWFpbC5jb20iLCJpYXQiOjE3NjQ5Njk0NDYsImV4cCI6MTc2NjE3OTA0Nn0.tUdjWI_dZH8m9oP1vjUxOK1CzaNn3BT-edUgD-ddbXMi-xwKjMLmRW0IUdIQK-t-27wrtq4eM65Nrt5sCpovfUzdpg6_EsW7d6LOxrugwDycKwQ54GiYoDQTPHP9mWHcgsrKvNVtIrWFWJufSgSuu0x2ofuQtejn_PSJvvJXBi-FI2zhN2JdDXrxIqCVcR8pXlvPpP3midishTBaoup2lUYifTyhDmV347j0W8ggu8PP2Ukuz1j8k72yTEf-NkytFTTGtjE3apL8x2IBsBLCrN8soFSnACjlc-Ca4x7oQ0RnmnDBKp76f5YBrcu3KflMVGCtfCk4diqQFHWirNRdLg"
Sys.setenv(OPENGWAS_JWT = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImFwaS1qd3QiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhcGkub3Blbmd3YXMuaW8iLCJhdWQiOiJhcGkub3Blbmd3YXMuaW8iLCJzdWIiOiJ3LnBhdWxpbmVnYWNoYW5qYUBnbWFpbC5jb20iLCJpYXQiOjE3NjQ5Njk0NDYsImV4cCI6MTc2NjE3OTA0Nn0.tUdjWI_dZH8m9oP1vjUxOK1CzaNn3BT-edUgD-ddbXMi-xwKjMLmRW0IUdIQK-t-27wrtq4eM65Nrt5sCpovfUzdpg6_EsW7d6LOxrugwDycKwQ54GiYoDQTPHP9mWHcgsrKvNVtIrWFWJufSgSuu0x2ofuQtejn_PSJvvJXBi-FI2zhN2JdDXrxIqCVcR8pXlvPpP3midishTBaoup2lUYifTyhDmV347j0W8ggu8PP2Ukuz1j8k72yTEf-NkytFTTGtjE3apL8x2IBsBLCrN8soFSnACjlc-Ca4x7oQ0RnmnDBKp76f5YBrcu3KflMVGCtfCk4diqQFHWirNRdLg")


Sys.getenv("OPENGWAS_JWT")

#####
#clumping
clumped_exp <- clump_data(exp_dat, clump_r2 = 0.01, pop = "EAS")

#######################################
#age at mernarche
exposure_AAM <- extract_instruments("ieu-a-1095") 
outcome_AAM <- extract_outcome_data(
  snps = exposure_AAM$SNP,
  outcomes = "ieu-a-1095"
)

#######################################
#type 2 diabetes
#ebi-a-GCST005047 morris et al
i
exposure_T2D <- extract_instruments("ebi-a-GCST005047") 
outcome_t2d <- extract_outcome_data(
  snps = exp$SNP,
  outcomes = "ebi-a-GCST005047"
)
#######################################
#heart failure

exposure_HF <- extract_instruments("ebi-a-GCST009541") 
outcome_t2d <- extract_outcome_data(
  snps = exp$SNP,
  outcomes = "ebi-a-GCST007517"
)
#######################################
#Lipids
#TG -> ebi-a-GCST002216
#TC -> ebi-a-GCST002221
#LDL -> ebi-a-GCST002222
#HDL -> ebi-a-GCST002223
####TG

exposure_TG <- extract_instruments("ebi-a-GCST002216")
if(nrow(exposure_TG) == 0) stop("No instruments for TG")
outcome_TG <- extract_outcome_data(snps = exposure_TG$SNP,
                                   outcomes = "ebi-a-GCST002216")

####TC
exposure_TC <- extract_instruments("ebi-a-GCST002221")
if(nrow(exposure_TC) == 0) stop("No instruments for TC")
outcome_TC <- extract_outcome_data(snps = exposure_TC$SNP,
                                   outcomes = "ebi-a-GCST002221")

####LDL
exposure_LDL <- extract_instruments("ebi-a-GCST002222")
if(nrow(exposure_LDL) == 0) stop("No instruments for LDL")
outcome_LDL <- extract_outcome_data(snps = exposure_LDL$SNP,
                                    outcomes = "ebi-a-GCST002222")

####HDL
exposure_HDL <- extract_instruments("ebi-a-GCST002223")
if(nrow(exposure_HDL) == 0) stop("No instruments for HDL")
outcome_HDL <- extract_outcome_data(snps = exposure_HDL$SNP,
                                    outcomes = "ebi-a-GCST002223")

)


# 1. Instruments for menopause
exp_meno <- extract_instruments("YOUR-MENOPAUSE-ID")

# 2. Outcome: menopause → TG
out_TG <- extract_outcome_data(
  snps     = exp_meno$SNP,
  outcomes = "ebi-a-GCST002216"  # TG
)

# 3. Outcome: menopause → TC
out_TC <- extract_outcome_data(
  snps     = exp_meno$SNP,
  outcomes = "ebi-a-GCST002221"  # TC
)

# 4. Outcome: menopause → LDL
out_LDL <- extract_outcome_data(
  snps     = exp_meno$SNP,
  outcomes = "ebi-a-GCST002222"  # LDL
)

# 5. Outcome: menopause → HDL
out_HDL <- extract_outcome_data(
  snps     = exp_meno$SNP,
  outcomes = "ebi-a-GCST002223"  # HDL
)

if(nrow(mv_dat_bmi_aam_to_lipid) > 3){
  res_mvmr <- mv_multiple(mv_dat_bmi_aam_to_lipid)
} else {
  res_mvmr <- NA
}
