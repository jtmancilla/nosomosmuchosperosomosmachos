load  <-  function()
{
    if (file.exists('algas.txt')){
        algas  <-  read.table("algas.txt",header = FALSE,
                              dec = ".",
                              col.names = c('temporada', 'tamaño', 'velocidad', 'mxPH',
                                            'mnO2', 'Cl', 'NO3', 'NO4', 'oPO4', 'PO4',
                                            'Chla', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 
                                            'a7'), na.strings = "XXXXXXX")
    }
    else{
        algas <- read.table(file = "~/itam-dm/data/algas/algas.txt", 
                            header = FALSE,
                            dec = ".",
                            col.names = c('temporada', 'tamaño', 'velocidad', 'mxPH',
                                          'mnO2', 'Cl', 'NO3', 'NO4', 'oPO4', 'PO4',
                                          'Chla', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7'),
                            na.strings=c('XXXXXXX'))
    }
    algas
}

