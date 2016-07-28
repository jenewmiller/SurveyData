# Jeanette Newmiller
# test zone

# Random message generation at action button press
sheetRiddles <- gs_key("1S5PSwSPU8co-EnkvII6BKO0DPxAOBtzjc5nCS5hpEx0")
dtaRiddles <- gs_read(sheetRiddles)

n1 <-  sample( x = (1:nrow(dtaRiddles))
            ,size = 1
            )
n2 <-  sample( x = (1:nrow(dtaRiddles))
            ,size = 1
            )


cat(dtaRiddles[[2]][n])

cat(dtaRiddles[[3]][n])

ind <- NA

if(is.na(ind)){
    cat("")
}else{
    cat(dtaRiddles[[3]][ind])

}
ind <- sample( x = (1:nrow(dtaRiddles))
            ,size = 1
            )
cat(dtaRiddles[[2]][ind])



