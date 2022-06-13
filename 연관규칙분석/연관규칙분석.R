
# 라이브러리 패키지
library(arules)
library(arulesViz)
library(RColorBrewer)
library(arulesViz)
library(igraph)

# 데이터 호출
data("Epub")
head(Epub)
summary(Epub)
str(Epub)

# 문서의 본문을 보는 함수 inspect
inspect(Epub[1 : 10])

# 아이템 10개의 품목이 차지하는 비율을 확인
itemFrequency(Epub[, 1:10], col = "red" )


# support(지지도) 1% 이상의 품목에대 대해서 막대그래프 생성
windows(height = 8, width = 8)
itemFrequencyPlot(Epub, support = 0.01, main = " Item plot aboce support 1%  (지지도 0.01이상 품목) ")

# 지지도 상위 20개 품목
itemFrequencyPlot(Epub, topN = 20, main = "support top 20 item (지지도 상위 20 개 품목) ", col=brewer.pal(8,'Pastel1'))

# 최소 지지도 기준을 0.001로 연관규칙분석 수행

if(0)`
Apriori 
- 가능한 모든 경우의 수를 탐색하는 방식을 개선하기 위해 데이터들의 발생빈도가 높은 것(빈발항목)을 찾는 알고리즘이다.      
- 최소 지지도보다 큰 지지도 값을 갖는 빈발항목 집합에 대해서만 연관규칙을 계산하는 알고리즘
`

Epub.rules <- apriori( data = Epub, parameter = list(support = 0.001, confidence = 0.20, minlen = 2))

# 65개의 규칙을 구성된 것을 확인할 수 있다.
summary(Epub.rules)

# 요약 결과 62개의 규칙이 2개 품목으로 이루어져 있고, 3개의 규칙은 3개의 품목으로 구성되어 있다.
# 향상도 최소값이 11.19로써 전반적으로 상당히 높은 것을 볼 수 있다.

# 향상도를 기준으로 상위 10개 연관규칙을 정렬해 확인하기
inspect(sort(Epub.rules, by = "lift")[1:10])

if(0)`
향상도를 기준으로 내림차순 정렬한 후 상위 3개의 규칙을 보면, 
rhs의 품목만 구매할 확률에 비해 lhs의 품목을 구매했을 때 rhs 품목도 구매할 확률이 400배 이상 높다 (Lift > 400)
따라서 rhs와 lhs의 품목들 간 결합상품 할인쿠폰 혹은 품목배치 변경(진열대 위치 변경) 등을 통한 매출 증대를 꾀할 수 있다.
`

plot(Epub.rules, col = c("blue", "salmon") )
# 결과 : Confidence(신뢰도)와 Lift(향상도)가 높은 경우 Support(지지도)는 낮은 경향이 보이는 것을 알 수 있다.
# Confidence(신뢰도)와 Lift(향상도)가 낮을 경우에는 Supoort(지지도) 또한 낮음


plot(Epub.rules[1:10], method = "graph" ,
     control = list(type = "items"), 
     col = c("blue", "salmon"),
    vertex.label.cex = 0.7, 
    edge.arrow.size = 0.3, 
    edge.arrow.width = 2,
    vertex.color = "green",
)
