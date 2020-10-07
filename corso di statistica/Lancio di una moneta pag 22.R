set.seed(203818)

money = c("TESTA","CROCE")

sample(money, 1)
#"CROCE"
sample(money, 5)
#Error in sample.int(length(x), size, replace, prob) : 
#  cannot take a sample larger than the population when 'replace = FALSE'
sample(money, 5, TRUE)
# "TESTA" "TESTA" "CROCE" "TESTA" "TESTA"
sample(money, 20, TRUE, c(0.8,0.2))
#"TESTA" "TESTA" "TESTA" "TESTA" "TESTA" "TESTA" "TESTA" "TESTA" "TESTA" "TESTA" "CROCE" "TESTA" "TESTA"
#"TESTA" "TESTA" "CROCE" "TESTA" "TESTA" "TESTA" "TESTA"


#end
