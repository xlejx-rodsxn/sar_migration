# SAR vessels data
study_period <- seq(mdy("JAN 01, 2009"), mdy("Oct 31, 2021"), by="days")
df_dates <- data.frame(study_period)
SAR_SeaWatch123 <- mdy("Jun 20,2015") - mdy("Mar 26, 2021")
SAR_SeaWatch4 <- mdy("Aug 15, 2020") - mdy("Oct 31, 2021")
SAR_Lifeline <- mdy("Sep 14,2017") - mdy("Jun 01, 2018")
SAR_SeaEye_TheSeaEye <- mdy("Apr 04, 2016") - mdy("Jun 21, 2018")
SAR_SeaEye_TheSeefuchs <- mdy("May 18,2017") - mdy("Jun 08, 2018")
SAR_SeaEye_AlanKurdi <- mdy("Dec 21,2018") - mdy("Sep 25, 2020")
SAR_OpenArms_Astral <- mdy("Jul 05,2016") - mdy("Oct 31, 2021")
SAR_OpenArms_GolfoAzzuro <- mdy("Dec 01,2016") - mdy("Dec 01, 2017")
SAR_OpenArms_OpenArms <- mdy("Dec 01,2017") - mdy("Apr 17, 2021")
SAR_MareLiberum <- mdy("Aug 26, 2018") - mdy("Oct 31, 2021")
SAR_Mediterranea <- mdy("Oct 03, 2018") - mdy("Oct 31, 2021")
SAR_SMH <- mdy("Oct 01, 2018") - mdy("Oct 31, 2021")
SAR_LouiseMichel <- mdy("Aug 01, 2020") - mdy(" Oct 01, 2020")
SAR_RefugeeRescue <- mdy("Jan 15, 2016") - mdy("Aug 14, 2020")
SAR_MOAS <- mdy("Aug 26, 2014") - mdy("Sep 06 2017")
SAR_JugendRettet <- mdy("Jul 24, 2016") - mdy("Aug 01, 2017")
SAR_MSFandSOS <- mdy("March 07, 2016") - mdy("Oct 31, 2021")
SAR_MSF_BourbonArgos<- mdy("May 09, 2015") - mdy("Nov 20, 2016")
SAR_MSF_Dignity1 <- mdy("Jun 13,2015") - mdy("Oct 04, 2016")
SAR_MSF_VosPrudence <- mdy("Mar 20, 2017") - mdy("Oct 05, 2017")
SAR_Resqship <- mdy("Apr 01,2019") - mdy("Oct 28, 2019")
SAR_Lifeboat <- mdy("Jul 01, 2016") - mdy("Sep 22, 2017")
FRONTEX_op_POSEIDON <- mdy("Jan 01, 2010") - mdy("Apr 28,2021")
FRONTEX_op_HERA <- mdy("Jan 01, 2010") - mdy("Apr 28,2021")
FRONTEX_op_TRITON <- mdy("Nov 01, 2014") - mdy("Feb 01, 2018")
FRONTEX_op_THEMIS <- mdy("Feb 01, 2018") - mdy("Apr 28,2021")
FRONTEX_op_MINERVA <- mdy("Jan 01, 2010") - mdy("Apr 28,2021")
EUNAVFOR_SOPHIA <- mdy("Jun 22, 2015") - mdy("Mar 31,2020")
EUNAVFOR_IRINI <- mdy("Mar 31, 2020") - mdy("Apr 28,2021")
MARE_NOSTRUM <- mdy("Oct 18, 2013") - mdy("Oct 27,2014")
COASTGUARD_LIBYA <- mdy("Feb 02, 2017") - mdy("Oct 31, 2021")

df_dates <- df_dates %>% 
  mutate(SAR_SeaWatch123 = case_when(study_period < mdy("Jun 20 ,2015") ~ 0,
                                     study_period %in% seq(mdy("Jun 20,2015"), mdy("Jul 01, 2018"), by = "days") ~ 1,
                                     study_period %in% seq(mdy("Jul 02,2018"), mdy("Oct 20, 2018"), by = "days") ~ 0,
                                     study_period %in% seq(mdy("Oct 21,2018"), mdy("Jan 30, 2019"), by = "days") ~ 1,
                                     study_period %in% seq(mdy("Jan 31,2019"), mdy("Feb 22, 2019"), by = "days") ~ 0,
                                     study_period %in% seq(mdy("Feb 23,2019"), mdy("May 17, 2019"), by = "days") ~ 1,
                                     study_period %in% seq(mdy("May 18,2015"), mdy("Jun 01, 2019"), by = "days") ~ 0,
                                     study_period %in% seq(mdy("Jun 02,2019"), mdy("Jun 28, 2019"), by = "days") ~ 1,
                                     study_period %in% seq(mdy("Jun 29,2019"), mdy("Dec 30, 2019"), by = "days") ~ 0,
                                     study_period %in% seq(mdy("Dec 31,2019"), mdy("Feb 27, 2020"), by = "days") ~ 1,
                                     study_period %in% seq(mdy("Feb 28,2020"), mdy("Jun 06, 2020"), by = "days") ~ 0,
                                     study_period %in% seq(mdy("Jun 07,2020"), mdy("Jun 16, 2020"), by = "days") ~ 1,
                                     study_period %in% seq(mdy("Jun 17,2020"), mdy("Feb 24, 2021"), by = "days") ~ 0,
                                     study_period %in% seq(mdy("Feb 25,2021"), mdy("Mar 26, 2021"), by = "days") ~ 1,
                                     study_period > mdy("Mar 26, 2021") ~ 0),
         SAR_SeaWatch4 = case_when(study_period < mdy("Aug 15, 2020") ~0,
                                   study_period %in% seq(mdy("Aug 15, 2020"), mdy("Sep 19,2020"), by = "days") ~1,
                                   study_period %in% seq(mdy("Sep 20, 2020"), mdy("Mar 02, 2021"), by = "days") ~0,
                                   study_period %in% seq(mdy("Mar 03, 2021"), mdy("Oct 31, 2021"), by ="days") ~1,
                                   study_period > mdy("Apr 28,2021") ~ 0),
         SAR_Lifeline = case_when(study_period < mdy("Sep 14,2017") ~ 0,
                                  study_period %in% seq(mdy("Sep 14,2017"), mdy("Jun 26, 2018"), by = "days") ~ 1,
                                  study_period %in% seq(mdy("Jun 27,2018"), mdy("Aug 25, 2019"), by = "days") ~ 0,
                                  study_period %in% seq(mdy("Aug 26,2019"), mdy("Sep 02, 2019"), by = "days") ~ 1,
                                  study_period > mdy("Sep 02, 2019") ~ 0),
         SAR_SeaEye_TheSeaEye = case_when(study_period < mdy("Apr 19, 2016") ~ 0,
                                          study_period %in% seq(mdy("Apr 19, 2016"), mdy("Jun 21, 2018"), by = "days") ~ 1,
                                          study_period > mdy("Jun 21, 2018") ~ 0),
         SAR_SeaEye_TheSeefuchs = case_when(study_period < mdy("May 18, 2017") ~ 0,
                                            study_period %in% seq(mdy("May 18, 2017"), mdy("Jun 06, 2018"), by = "days") ~ 1,
                                            study_period > mdy("Jun 06, 2018") ~ 0),
         SAR_SeaEye_AlanKurdi = case_when(study_period < mdy("Dec 21,2018") ~ 0,
                                          study_period %in% seq(mdy("Dec 21, 2018"), mdy("May 04, 2020"), by = "days") ~ 1,
                                          study_period %in% seq(mdy("May 05, 2020"), mdy("Sep 11, 2020"), by = "days") ~ 0,
                                          study_period %in% seq(mdy("Sep 12, 2020"), mdy("Sep 25, 2020"), by = "days") ~ 1,
                                          study_period > mdy("Sep 25, 2020") ~ 0),
         SAR_OpenArms_Astral = case_when(study_period < mdy("Jun 01,2016") ~ 0,
                                         study_period %in% seq(mdy("Jun 01,2016"), mdy("Oct 31, 2021"), by = "days") ~ 1,
                                         study_period > mdy("Oct 31, 2021") ~ 0) , 
         SAR_OpenArms_GolfoAzzuro = case_when(study_period < mdy("Dec 02,2016") ~ 0,
                                              study_period %in% seq(mdy("Dec 01,2016"), mdy("Dec 01, 2017"), by = "days") ~ 1,
                                              study_period > mdy("Dec 01, 2017") ~ 0), 
         SAR_OpenArms_OpenArms = case_when(study_period < mdy("Dec 01,2017") ~ 0,
                                           study_period %in% seq(mdy("Dec 01, 2017"), mdy("Mar  16, 2018"), by = "days") ~ 1, 
                                           study_period %in% seq(mdy("Mar 17, 2018"), mdy("Apr  16, 2018"), by = "days") ~ 0, 
                                           study_period %in% seq(mdy("Apr 17, 2018"), mdy("Jan 13, 2019"), by = "days") ~ 1,
                                           study_period %in% seq(mdy("Jan 14, 2019"), mdy("Apr  23, 2019"), by = "days") ~ 0,
                                           study_period %in% seq(mdy("Apr 24, 2019"), mdy("Aug  20, 2019"), by = "days") ~ 1,
                                           study_period %in% seq(mdy("Aug 21, 2019"), mdy("Aug 29, 2019"), by = "days") ~ 0,
                                           study_period %in% seq(mdy("Aug 30, 2019"), mdy("Apr  17, 2021"), by = "days") ~ 1,
                                           study_period %in% seq(mdy("Aug 26 ,2018"), mdy("Apr 23, 2019"), by = "days") ~ 1,
                                           study_period %in% seq(mdy("Apr 24, 2019"), mdy("May 13, 2019"), by = "days") ~ 0,
                                           study_period %in% seq(mdy("May 14, 2019"), mdy("Aug 18, 2020"), by = "days") ~ 1,
                                           study_period %in% seq(mdy("Aug 19, 2020"), mdy("Oct 02, 2020"), by = "days") ~ 0,
                                           study_period %in% seq(mdy("Oct 03, 2020"), mdy("Oct 31, 2021"), by = "days") ~ 1,
                                           study_period > mdy("Oct 31, 2021") ~ 0),
         SAR_Mediterranea = case_when(study_period < mdy("Oct 03, 2018") ~ 0,
                                      study_period %in% seq(mdy("Oct 03,2018"), mdy("Mar 18, 2019"), by = "days") ~ 1,
                                      study_period %in% seq(mdy("Mar 19, 2019"), mdy("Mar 27, 2019"), by = "days") ~ 0,
                                      study_period %in% seq(mdy("Mar 28, 2019"), mdy("May 09, 2019"), by = "days") ~ 1,
                                      study_period %in% seq(mdy("May 10, 2019"), mdy("Jul 01, 2019"), by = "days") ~ 0,
                                      study_period %in% seq(mdy("Jul 02, 2019"), mdy("Jul 06, 2019"), by = "days") ~ 1,
                                      study_period %in% seq(mdy("Jul 07, 2019"), mdy("Aug 22, 2019"), by = "days") ~ 0,
                                      study_period %in% seq(mdy("Aug 23, 2019"), mdy("Sep 01, 2019"), by = "days") ~ 1,
                                      study_period %in% seq(mdy("Sep 02, 2019"), mdy("Feb 04, 2020"), by = "days") ~ 0,
                                      study_period %in% seq(mdy("Feb 05, 2020"), mdy("Mar 19, 2020"), by = "days") ~ 1,
                                      study_period %in% seq(mdy("Mar 19, 2020"), mdy("Jun 09, 2020"), by = "days") ~ 0,
                                      study_period %in% seq(mdy("Jun 10, 2020"), mdy("Sep 25, 2020"), by = "days") ~ 1,
                                      study_period > mdy("Sep 25, 2020") ~ 0),
         SAR_SMH =case_when(study_period < mdy("Oct 01, 2018") ~ 0,
                            study_period %in% seq(mdy("Oct 01, 2018"), mdy("Jan 17, 2019"), by = "days") ~ 1,
                            study_period %in% seq(mdy("Jan 18, 2019"), mdy("Apr 17, 2019"), by = "days") ~ 0,
                            study_period %in% seq(mdy("Apr 18, 2019"), mdy("May 07, 2020"), by = "days") ~ 1,
                            study_period %in% seq(mdy("May 08, 2020"), mdy("Dec 08, 2020"), by = "days") ~ 0,
                            study_period %in% seq(mdy("Dec 09, 2020"), mdy("Oct 31, 2021"), by = "days") ~ 1,
                            study_period > mdy("Oct 31, 2021") ~ 0),
         SAR_LouiseMichel = case_when(study_period < mdy("Aug 22, 2020") ~ 0,
                                      study_period %in% seq(mdy("Aug 22, 2020"), mdy("Oct 22, 2020"), by ="days") ~1,
                                      study_period >mdy("Oct 22, 2020") ~ 0),
         SAR_RefugeeRescue = case_when(study_period < mdy("Jan 15 2016") ~ 0,
                                       study_period %in% seq(mdy("Jan 15, 2016"), mdy("Aug 14, 2020"), by ="days") ~ 1,
                                       study_period >mdy("Aug 14, 2020") ~ 0),
         SAR_MOAS = case_when(study_period < mdy("Aug 26, 2014") ~ 0,
                              study_period %in% seq(mdy("Aug 26, 2014"), mdy("Sep 06, 2017"), by ="days") ~ 1,
                              study_period >mdy("Sep 06, 2017") ~ 0),
         SAR_JugendRettet = case_when(study_period < mdy("Jul 24, 2016") ~ 0,
                                      study_period %in% seq(mdy("Jul 24, 2016"), mdy("Aug 01, 2017"), by ="days") ~ 1,
                                      study_period >mdy("Aug 01, 2017") ~ 0),
         SAR_MSFandSOS = case_when(study_period < mdy("May 09, 2015") ~ 0,
                                   study_period %in% seq(mdy("May 09, 2015"), mdy("Nov 19, 2018"), by ="days") ~ 1,
                                   study_period %in% seq(mdy("Nov 20, 2018"), mdy("Jul 20, 2019"), by ="days") ~ 0,
                                   study_period %in% seq(mdy("Jul 21, 2019"), mdy("Oct 31, 2021"), by ="days") ~ 1,
                                   study_period > mdy("Oct 31, 2021") ~ 0),
         SAR_SavetheChildren = case_when(study_period < mdy("Sep 08, 2016") ~ 0,
                                         study_period %in% seq(mdy("Sep 08, 2016"), mdy("Oct 23, 2017"), by ="days") ~ 1,
                                         study_period >mdy("Oct 23, 2017") ~ 0),
         SAR_MSF_BourbonArgos = case_when(study_period < mdy("May 09, 2015") ~ 0,
                                          study_period %in% seq(mdy("May 09, 2015"), mdy("Aug 16, 2015"), by ="days") ~ 1,
                                          study_period %in% seq(mdy("Aug 17, 2015"), mdy("Oct 02, 2015"), by ="days") ~ 0,
                                          study_period %in% seq(mdy("Oct 03, 2015"), mdy("Jan 14, 2016"), by ="days") ~ 1,
                                          study_period %in% seq(mdy("Jan 15, 2016"), mdy("May 05, 2016"), by ="days") ~ 0,
                                          study_period %in% seq(mdy("May 06, 2016"), mdy("Nov 20, 2016"), by ="days") ~ 1,
                                          study_period > mdy("Nov 20, 2016") ~ 0),
         SAR_MSF_Dignity1 = case_when(study_period < mdy("Jun 13, 2015") ~ 0,
                                      study_period %in% seq(mdy("Jun 13, 2015"), mdy("Nov 04, 2015"), by ="days") ~ 1,
                                      study_period %in% seq(mdy("Nov 05, 2015"), mdy("Apr 21, 2016"), by ="days") ~ 0,
                                      study_period %in% seq(mdy("Apr 22, 2016"), mdy("Oct 04, 2016"), by ="days") ~ 1,
                                      study_period > mdy("Oct 04, 2016") ~ 0),
         SAR_MSF_VosPrudence = case_when(study_period < mdy("Mar 20, 2017") ~ 0,
                                         study_period %in% seq(mdy("Mar 20, 2017"), mdy("Oct 05, 2017"), by ="days") ~ 1, 
                                         study_period > mdy("Oct 05, 2017") ~ 0),
         SAR_Resqship = case_when(study_period < mdy("Apr 01, 2019") ~ 0,
                                  study_period %in% seq(mdy("Apr 01, 2019"), mdy("Oct 28, 2019"), by ="days") ~ 1, 
                                  study_period > mdy("Oct 28, 2019") ~ 0),
         SAR_Lifeboat = case_when(study_period < mdy("Jul 01, 2016") ~ 0,
                                  study_period %in% seq(mdy("Jul 01, 2016"), mdy("Sep 22, 2017"), by ="days") ~ 1, 
                                  study_period > mdy("Sep 22, 2017") ~ 0),
         FRONTEX_op_POSEIDON = case_when(study_period < mdy("Jan 01, 2010") ~ 0,
                                         study_period %in% seq(mdy("Jan 01, 2010"), mdy("Oct 31, 2021"), by ="days") ~1,
                                         study_period >mdy("Oct 31, 2021") ~ 0),
         FRONTEX_op_HERA = case_when(study_period < mdy("Jan 01, 2010") ~ 0,
                                     study_period %in% seq(mdy("Jan 01, 2010"), mdy("Oct 31, 2021"), by ="days") ~1,
                                     study_period >mdy("Oct 31, 2021") ~ 0),
         FRONTEX_op_MINERVA = case_when(study_period < mdy("Jan 01, 2010") ~ 0,
                                        study_period %in% seq(mdy("Jan 01, 2010"), mdy("Oct 31, 2021"), by ="days") ~1,
                                        study_period >mdy("Oct 31, 2021") ~0),
         FRONTEX_op_TRITON = case_when(study_period < mdy("Nov 01, 2014") ~ 0,
                                       study_period %in% seq(mdy("Nov 01, 2014"), mdy("Feb 01,2018"), by ="days") ~1,
                                       study_period >mdy("Feb 01, 2018") ~0),
         FRONTEX_op_THEMIS = case_when(study_period < mdy("Feb 01, 2018") ~ 0,
                                       study_period %in% seq(mdy("FEB 01, 2018"), mdy("Oct 31, 2021"), by ="days") ~1,
                                       study_period >mdy("Oct 31, 2021") ~ 0),
         EUNAVFOR_SOPHIA = case_when(study_period < mdy("Jun 22, 2015") ~ 0,
                                     study_period %in% seq(mdy("Jun 22, 2015"), mdy("Mar 31, 2020"), by ="days") ~1,
                                     study_period >mdy("Mar 31, 2020") ~ 0),
         EUNAVFOR_IRINI = case_when(study_period < mdy("Mar 31, 2020") ~ 0,
                                    study_period %in% seq(mdy("Mar 31, 2020"), mdy("Oct 31, 2021"), by ="days") ~1,
                                    study_period >mdy("Oct 31, 2021") ~ 0),
         MARE_NOSTRUM = case_when(study_period < mdy("Oct 18, 2013") ~ 0,
                                  study_period %in% seq(mdy("Oct 18, 2013"), mdy("Oct 27, 2014"), by ="days") ~ 1,
                                  study_period >mdy("Oct 27, 2014") ~ 0),
         COASTGUARD_LIBYA = case_when(study_period < mdy("Feb 02, 2017") ~ 0,
                                      study_period %in% seq(mdy("Feb 02, 2017"), mdy("Oct 31, 2021"), by ="days") ~1,
                                      study_period >mdy("Oct 31, 2021") ~ 0),
         Extension_SAR_LIBYA = case_when(study_period < mdy("Jun 08, 2018") ~ 0,
                                         study_period >= mdy("Jun 08, 2018") ~ 1)
  )

rm(study_period,
   SAR_SeaWatch123,
   SAR_SeaWatch4,
   SAR_Lifeline,
   SAR_SeaEye_TheSeaEye,
   SAR_SeaEye_TheSeefuchs,
   SAR_SeaEye_AlanKurdi,
   SAR_OpenArms_Astral,
   SAR_OpenArms_GolfoAzzuro,
   SAR_OpenArms_OpenArms,
   SAR_MareLiberum,
   SAR_Mediterranea,
   SAR_SMH,
   SAR_LouiseMichel,
   SAR_RefugeeRescue,
   SAR_MOAS,
   SAR_JugendRettet,
   SAR_MSFandSOS,
   SAR_MSF_BourbonArgos,
   SAR_MSF_Dignity1,
   SAR_MSF_VosPrudence,
   SAR_Resqship,
   SAR_Lifeboat,
   FRONTEX_op_POSEIDON,
   FRONTEX_op_HERA,
   FRONTEX_op_TRITON,
   FRONTEX_op_THEMIS,
   FRONTEX_op_MINERVA,
   EUNAVFOR_SOPHIA,
   EUNAVFOR_IRINI,
   MARE_NOSTRUM,
   COASTGUARD_LIBYA)