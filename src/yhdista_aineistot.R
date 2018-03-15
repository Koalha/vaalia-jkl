## Yhdistetaan aineistot
library(tidyverse)
library(lme4)
library(rstanarm)

## Sekamalli, ehdokas on sekatekijä
## Äänisaalis on jakautunut ylidispersoituneen biomiaalisesti
## Ensit alueanalyysi
kv <- read_csv("../data/kuntavaalit_alueet_keskisuomi.csv")
at <- read_csv("../data/aanestysalueet_aanestystiedot_sukupuoli.csv") %>% filter(kunta_suom == "Jyväskylä")
tulot <- read_csv("../data/kunnat_tulot.csv")
kt <- read_csv("../data/kuntien_tunnuslukuja.csv")



jkl_alue <- kv %>% filter(kunta == 179) %>% select(-aanestysalue) %>% mutate(aanestysalue = aanestysalue_suom) %>% left_join(at, by = "aanestysalue")

tst <- jkl_alue %>% select(aanet_yht,
                           aanestaneet,
                           aanestaneet_naiset,
                           aanioikeutetut,
                           kunta_suom,
                           aanestysalue_suom,
                           puolue_lyh,
                           sukupuoli,
                           ehdokasnro,
                           etunimi,
                           sukunimi,
                           europarlamentaarikko,
                           kansanedustaja,
                           kunnanvaltuutettu)

tst <- tst %>% mutate(aaniosuus = aanet_yht/aanestaneet,
                      naisaantenosuus = aanestaneet_naiset/aanestaneet,
                      aanestyspros = aanestaneet/aanioikeutetut)

tst <- tst %>% spread(ehdokasnro,aanet_yht,fill=0)
tst <- tst %>% gather("ehdokasnro","aanet_yht", -c(1:15))

m <- stan_glmer(aaniosuus~puolue_lyh + (1|ehdokasnro),data = tst,family = "binomial")
