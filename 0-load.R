load  <-  function()
{
    if (file.exists('algas.txt')){
        algas  <-  read.table("algas.txt", na.strings = "XXXXXXX")
    }
    else{
        algas  <-  read.table("~/itam-dm/data/algas/algas.txt", na.strings = "XXXXXXX")
    }
    algas
}

