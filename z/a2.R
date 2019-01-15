library(tm)
library(KoNLP)

docs <- 
  c("사랑은 달콤한 꽃이나 그것을 따기 위해서는 무서운 벼랑 끝까지 갈 용기가 있어야 한다.", 
    "진실한 사랑의 실체는 믿음이다.",
    "눈물은 눈동자로 말하는 고결한 언어.",
    "친구란 두 사람의 신체에 사는 하나의 영혼이다.",
    "흐르는 강물을 잡을수 없다면, 바다가 되어서 기다려라.",
    "믿음 소망 사랑 그중에 제일은 사랑이라.",
    "가장 소중한 사람은 가장 사랑하는 사람이다.",
    "사랑 사랑 사랑")

#편의상 검색어도 넣어준다. 
query <- "믿음을 주는 사랑"

names(docs) <- paste("doc", 1:length(docs), sep="")
docs <- c(docs, query=query)
docs.corp <- Corpus(VectorSource(docs))

#색인어 추출함수 
konlp_tokenizer <- function(doc){
  extractNoun(doc)
}

# weightTfIdf 함수 말고 다른 여러 함수들이 제공되는데 관련 메뉴얼을 참고하길 바란다. 
tdmat <- TermDocumentMatrix(docs.corp, control=list(tokenize=konlp_tokenizer,
                                                    weighting = function(x) weightTfIdf(x, TRUE),
                                                    wordLengths=c(1,Inf)))

tdmatmat <- as.matrix(tdmat)

# 벡터의 norm이 1이 되도록 정규화 
norm_vec <- function(x) {x/sqrt(sum(x^2))}
tdmatmat <- apply(tdmatmat, 2, norm_vec)

# 문서 유사도 계산 
docord <- t(tdmatmat[,9]) %*% tdmatmat[,1:8]

#검색 결과 리스팅 
orders <- data.frame(docs=docs[-9],scores=t(docord) ,stringsAsFactors=FALSE)
orders[order(docord, decreasing=T),]
