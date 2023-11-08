library(tidyverse)
library(crayon)
library(glue)

# Tabela 07/11/2023 -------------------------------------------------------

tab_init <- read_csv2(file = "data/tabelaBr20231107.csv",
                      locale  = readr::locale(
                        encoding = "latin1"
                      ))


# Rodadas -----------------------------------------------------------------

rodadas <- read_csv2(file = "data/rodadasFinais.csv",
                     locale  = readr::locale(
                       encoding = "latin1"
                     ))

# Seed --------------------------------------------------------------------

set.seed(42)
