library(gFirestoreR)
library(googleAuthR)

#Test documents
data1 <- list('test'=123, 'a'='abc', 'b'=c(0.2,0.3), 'c'=T)
data2 <- list('test'=c(1,2,3))
a <- listDocuments(cG='Directories', partial.path=T)
b <- getDocument(doc=a[[2]])
c <- createDocument(documentId = 'test2', data=data1, cG='Directories', partial.path=T)
d <- patchDocument(c, newData=data2, overwrite=T)

#Test multi write
d2 <- gFire.doc(name=paste0(d$name,'2'), fields=data2)
d$fields <- gFire.fields(data1)
writes <- list(gFire.writeObj(d2, method='update'), gFire.writeObj(d, method='update'))
e <- batchWriteDocuments(writes)

#Test multi delete
writes <- list(gFire.writeObj(d2, method='delete'))
e <- batchWriteDocuments(writes)

f <- deleteDocument(d)

