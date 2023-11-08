library(tidyverse)
library(crayon)

# Tabela 07/11/2023 -------------------------------------------------------

tab_init <- read_csv2(file = "data/tabelaBr20231107.csv")


# Rodadas -----------------------------------------------------------------

rodadas <- read_csv2(file = "data/rodadasFinais.csv")

# Seed --------------------------------------------------------------------

set.seed(42)
