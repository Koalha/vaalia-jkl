## Yhdistetaan aineistot
library(tidyverse)
library(broom)
library(rgdal)

## datat
kv <- read_csv("../data/kuntavaalit_alueet_keskisuomi.csv")
at <- read_csv("../data/aanestysalueet_aanestystiedot_sukupuoli.csv")
tulot <- read_csv("../data/kunnat_tulot.csv")
kt <- read_csv("../data/kuntien_tunnuslukuja.csv")
kunta_ala <- readOGR("../data/alueet", "keskisuomi_kunnat", stringsAsFactors = FALSE)@data[4:5]
kunta_ala$ala <- as.numeric(kunta_ala$ala)

at$aanestysalue_suom <- gsub("[[:digit:]]+ ", "", at$alue)
at$aanestysalue_suom

## Äänestysalue **** == koko kunnan äänet, voidaan poistaa.
kv <- kv %>% filter(!aanestysalue == "****")

## Eri resoluutioiden tarkastelussa pitää käyttää oikean resoluution selittäjiä
## Kuntataso = puolueet ja kunnat

kun <- kv %>%
    mutate(kunta_suom = kotikunta_suom) %>%
    group_by(kunta,kunta_suom,puolue_lyh) %>%
    summarize(aanet = sum(aanet_yht))

kun2 <- kv %>%
    mutate(kunta_suom = kotikunta_suom) %>%
    group_by(kunta,kunta_suom) %>%
    summarize(aanestaneet = sum(aanet_yht))

kun3 <- at %>%
    group_by(kunta,kunta_suom) %>%
    summarize(aanioikeutetut = sum(aanioikeutetut),
              aanioikeutetut_miehet = sum(aanioikeutetut_miehet),
              aanioikeutetut_naiset = sum(aanioikeutetut_naiset),
              aanestaneet_miehet = sum(aanestaneet_miehet),
              aanestaneet_naiset = sum(aanestaneet_naiset))

kun <- kun %>% left_join(kun2) %>% left_join(kun3) %>% left_join(kt) %>% left_join(kunta_ala) %>% left_join(tulot)

kun <- kun %>% filter(puolue_lyh %in% c("KD", "KESK", "KOK", "PS", "SDP","VAS", "VIHR")) %>% select(-vuosi)


## Minkälaisia kuntia on?
kunnat <- kun %>% ungroup %>% select(2,11:46) %>% unique
kunnat$asukastiheys <- kunnat$vakiluku/kunnat$ala
kunnat$ak_tulo_per_kulutusyksikko <- kunnat$ak_tulo_kaytettavissa_keskiarvo/kunnat$kulutusyksikoita_keskimaarin


## Valitaan korrelaatioilla tiettyjä muuttujia
kp <- prcomp(kunnat[-1],scale=TRUE)
plot(kp)
biplot(kp)

kunnat2 <- kunnat[c("huoltosuhde",
                    "asuntokunnan_keskikoko",
                    "ak_tulo_per_kulutusyksikko",
                    "muuttovoitto",
                    "rivi_pientaloissa_asuntokunnat",
                    "syntyneiden_enemmyys",
                    "taajama_aste",
                    "tyottomien_osuus_tyovoimasta",
                    "ulkomaalaisetosuus",
                    "vuokraasunnoissa_asuntokunnat",
                    "korkea_asteen_suorittaneet_15v",
                    "asukastiheys")]

kp2 <- prcomp(kunnat2,scale=TRUE)
plot(kp2)
biplot(kp2)

kunnat3 <- kunnat2 %>% select(-ulkomaalaisetosuus,-muuttovoitto,-syntyneiden_enemmyys,-taajama_aste,-rivi_pientaloissa_asuntokunnat)

kunnat3 %>%
    cor() %>%
    data.frame(var1=row.names(.),.) %>%
    gather(key="var2",value="cor",-1) %>%
    ggplot(aes(var1,var2)) +
    geom_tile(aes(fill=cor)) +
    geom_text(aes(label = round(cor,1))) +
    scale_fill_gradient2() +
    theme(axis.text.x = element_text(angle=45,hjust=1))
    

kp3 <- prcomp(kunnat3,scale=TRUE)
plot(kp3)
biplot(kp3)

kunnat3 <- data.frame(kunnat[1],kunnat3)


## Listan pituus dataan mukaan
listat <- kv %>%
    select(kunta,puolue_lyh,ehdokasnro) %>%
    unique %>%
    group_by(kunta,puolue_lyh) %>%
    summarize(lista_pituus = n()) %>%
    mutate(lista_taytto = lista_pituus/max(lista_pituus))

kun <- kun %>% left_join(listat)

## Äänestysprosentti ja naisten enemmyys äänestäneissä
kun <- kun %>% mutate(aanestyspros = aanestaneet/aanioikeutetut,
               aanestaneet_naistapermies = aanestaneet_naiset/aanestaneet_miehet)


## Korrelaatiot
pdf("../doc/muuttujien_korrelaatiot.pdf")
kun %>%
    ungroup %>%
    left_join(kunnat3 %>% select(kunta_suom,
                                 ak_tulo_per_kulutusyksikko,
                                 asukastiheys)) %>%
    select(aanestyspros,
           aanestaneet_naistapermies,
           names(kunnat3[-1])) %>%
    unique %>%
    cor() %>%
    data.frame(var1=row.names(.),.) %>%
    gather(key="var2",value="cor",-1) %>%
    ggplot(aes(var1,var2)) +
    geom_tile(aes(fill=cor)) +
    geom_text(aes(label = round(cor,1))) +
    scale_fill_gradient2(limit=c(-1,1)) +
    theme(axis.text.x = element_text(angle=45,hjust=1))
dev.off()    


## Missä on ääniä
aan <- kun %>%
    select(kunta_suom,aanioikeutetut) %>%
    unique %>%
    arrange(aanioikeutetut)

aan$kunta_suom <- factor(aan$kunta_suom, levels = rev(aan$kunta_suom), ordered  =TRUE)

pdf("../doc/aanioikeutetut.pdf",)
aan %>%
    ggplot(aes(kunta_suom,aanioikeutetut)) +
    geom_bar(stat ="identity") +
    labs(y = "Äänioikeutetut", x = "Kunta") + 
    theme(axis.text.x = element_text(angle=45,hjust=1))
dev.off()    

## Yksittäiset regressiot
## KUVAT!

for(i in c("KD", "KESK", "KOK", "PS", "SDP","VAS", "VIHR")){
    cat(i)
    fname <- paste0("../doc/muuttujien_suhde_",
                    i,
                    "_kannatukseen.pdf")
    pdf(fname,width=10,height=10)
    dd <-  kun %>%
        filter(puolue_lyh == i) %>%
        mutate(aaniosuus = aanet/aanestaneet) %>%
        left_join(kunnat3 %>% select(kunta_suom,
                                     ak_tulo_per_kulutusyksikko,
                                     asukastiheys)) %>%
        ungroup %>%
        select(aaniosuus,
               lista_taytto,
               aanestyspros,
               aanestaneet_naistapermies,
               names(kunnat3)) %>%
        gather("variable","value",-c("aaniosuus","kunta_suom"))
    dd$variable <- factor(dd$variable,
                          labels = c("Äänestäneitä naisia per mies",
                                     "Äänestysprosentti",
                                     "Asuntokunnan tulot per kulutusyksikkö",
                                     "Asukastiheys",
                                     "Asuntokunnan keskikoko",
                                     "Huoltosuhde",
                                     "Korkea-asteen suorittaneiden osuus 15-vuotiaista",
                                     "Listan täyttöaste",
                                     "Työttömien osuus työvoimasta",
                                     "Vuokra-asunnoissa asuvien osuus asuntokunnista"))    
    p <- dd %>% ggplot(aes(value,aaniosuus)) +
            geom_text(aes(label = substr(kunta_suom,0,3))) +
            stat_smooth(method = "lm") +
            facet_wrap(~variable,scale = "free_x") +
            ggtitle(i) +
            theme(strip.text = element_text(size = rel(0.65)))
    print(p)
    dev.off()
}
