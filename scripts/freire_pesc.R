ip_freire <- read_excel("~/Doutorado/fabricio_RN/data/FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx", 
                        sheet = "Reconstruction")


rn_art <- ip_freire |> 
  filter(OtherArea == "Rio Grande do Norte" & Sector == "Artisanal (SS, C)") |> 
  filter(Year == 2015) |> 
  select(PortugueseCommonName, TaxonName, CatchAmount_t)


pe_art <- ip_freire |> 
  filter(OtherArea == "Pernambuco" & Sector == "Artisanal (SS, C)") |> 
  filter(Year == 2015)|> 
  select(PortugueseCommonName, TaxonName, CatchAmount_t)

#write.xlsx(x = pe_art, file = "fish_species/pe_art.xlsx")
