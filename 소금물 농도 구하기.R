salt <- 50
water <- 100
print(salt)
print(water)

result <- salt / (salt + water) * 100

cat('소금 = ', salt, ' 물 =', water,' : 농도 =', result, '%')

salt <- 70
water <- 110
print(salt)
print(water)

result <- salt / (salt + water) * 100

cat('소금 = ', salt, ' 물 =', water,' : 농도 =', result, '%')

salt <- c(50, 70)
water <- c(100, 110)
result <- salt / (salt + water) * 100

for(i in 1:length(salt)){
  cat('소금 = ', salt[i], ' 물 =', water[i],' : 농도 =', result[i], '% \n')
}
  
