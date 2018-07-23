#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(wordspace)
library(plyr)

# initData <- function(){
#     if(!exists("MNIST")){
#         library(R.matlab)
#         library(mdatools)
#         MNIST <- readMat("mnist-original.mat")
#     }
#     return(MNIST)
# }
# MNIST <- initData()
#' qm for matrix
qm <- function(...) {
    # Get the arguments as a list
    arg <- eval(substitute(alist(...)))
    # Initialize l as a list of vecors, each vector in l corresponds to one row
    # of the matrix.
    l <- list(c())
    # rhl_l is a list where we will push the rhs of expressions like 1 | 2 | 3 ,
    # which parses as (1 | 2) | 3 , while we deal with the left hand side (1 |
    # 2)
    rhl_l <- list()
    while (length(arg) > 0) {
        a <- arg[[1]]
        arg <- tail(arg, -1)
        if (length(a) > 1 && a[[1]] == "|") {
            # Push the left hand side of the ... | ... expression back on the arguments
            # list and push the rhs onto rhl_l
            arg <- c(a[[2]], arg)
            rhl_l <- c(a[[3]], rhl_l)
        } else {
            # Just a normal element, that we'll evaluate and append to the last
            # vector/row in l.
            l[[length(l)]] <- c(l[[length(l)]], eval(a))
            # If there are rhs elements left in rhs_l we'll append them as new
            # vectors/rows on l and then we empty rhs_l.
            for (i in seq_along(rhl_l)) {
                l[[length(l) + 1]] <- eval(rhl_l[[i]])
            }
            rhl_l <- list()
        }
    }
    do.call(rbind, l)
}
#' assign
'%=%' = function(l, r, ...) {
    Envir = as.environment(-1)
    for (II in 1:length(l)) {
        do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
    }
}
#'
normalize <- function(X){
    mu = rowMeans(X) # X.mean(axis=0)
    std = apply(X,1,sd) #np.std(X, axis=0)
    std_filled = std
    std_filled[std==0] = 1.
    Xbar = (X - mu) / std_filled
    return(list(Xbar=Xbar, mu=mu, std=std))
}
#'
eig <- function(S){
    list("eigvals", "eigvecs") %=% eigen(S) 
    idx = order(eigvals,decreasing = T) # np.argsort(eigvals)[::-1]
    eigvals = eigvals[idx]
    eigvecs = eigvecs[,idx] #eigvecs[:,idx]
    return(list(eigvals=eigvals, eigvecs=eigvecs))
}
#'
projection_matrix <- function(B){
    P = B %*% solve(t(B) %*% B) %*% t(B) # B @ np.linalg.inv(B.T @ B) @ B.T
    return(P)
}
#'
PCA_origin <- function(X, num_components){
    cat("PCA function")
    c("N", "D") %=% dim(X)
    S = (t(X) %*% X) / N # D, D 
    list("eig_vals", "eig_vecs") %=% eig(S)
    NC = num_components
    NC=ifelse(NC>dim(eig_vecs)[2],dim(eig_vecs)[2],NC)
    P = projection_matrix(eig_vecs[,0:NC]) # projection matrix
    X_reconstruct = X %*% P
    return(X_reconstruct)
}
#'
PCA_high_dim <- function(X, num_components){
    cat("PCA_high_dim function")
    c("N", "D") %=% dim(X) # N, D = X.shape
    M = (X %*% t(X)) / N # N, N
    list("eig_vals", "eig_vecs") %=% eig(M)# EDIT THIS, compute the eigenvalues. 
    U = t(X) %*% eig_vecs # EDIT THIS. Compute the eigenvectors for the original PCA problem.
    NC = num_components
    NC=ifelse(NC>dim(U)[2],dim(U)[2],NC)
    P = projection_matrix(U[,0:NC]) # projection matrix
    X_reconstruct = X %*% P # np.zeros((N, D)) EDIT THIS.
    return(X_reconstruct)
}
#'
PCA_compress <- function(X, num_components){
    c("N", "D") %=% dim(X) # N, D = X.shape
    if (N > D) {
        S = (t(X) %*% X) / N
        list("eig_vals", "eig_vecs") %=% eig(S)
        NC = num_components
        NC = ifelse(NC>dim(eig_vecs)[2],dim(eig_vecs)[2],NC)
        P = projection_matrix(eig_vecs[,0:NC]) 
    }else{
        M = (X %*% t(X)) / N
        list("eig_vals", "eig_vecs") %=% eig(M)
        U = t(X) %*% eig_vecs 
        NC = num_components
        NC = ifelse(NC>dim(U)[2],dim(U)[2],NC)
        P = projection_matrix(U[,0:NC]) 
    }
    X_reconstruct = X %*% P # np.zeros((N, D)) EDIT THIS.
    return(X_reconstruct)
}
#'
distance_1D <- function(x, y){
    x = matrix(x) # np.array(x, dtype=np.float).ravel() 
    y = matrix(y) # np.array(y, dtype=np.float).ravel()
    distance = sqrt(t(x - y) %*% (x - y)) # np.sqrt((x-y) @ (x-y))
    return(distance)
}
dist_matrix_dist <- function(X){return(matrix(dist.matrix(t(X),
                                                          method = "euclidean",
                                                          as.dist = T))) } # N = sum(0:D) D dim 
dist_matrix <- function(X) {
    M = dist.matrix(t(X),method = "euclidean")
    return(matrix(M,dim(M)))
}
dist_matrix_idx <- function(x,X) {
    X = t(X) 
    D = data.frame()
    n = dim(X)[1]
    for(i in seq(n)){
        M = data.frame(dist=matrix(dist.matrix(rbind(X[x,], X[i,]), method = "euclidean",as.dist = T)))
        D = rbind(D,M)
    }
    return(D)
}
# Example:
# dist_matrix(qm(1,2,3|3,2,1|3,1,2|4,3,2))
#'
rotate <- function(x) t(apply(x, 2, rev))
digit_array <- function(X,N) return(X$data[,X$label==N]) # target digit to N
img_matrix <- function(A) matrix(A,28,28,byrow = T) # 
show_img <- function(M,idx=c("titles")) { 
    image(rotate(M),col = grey((0:32)/32),cex.axis=.5)
    title(sub = capture.output(cat("similar idx =",paste(tail(idx,-1),"|"))),col.lab="blue",
          xlab = capture.output(cat(" idx =",idx[1])),col.sub="darkblue",
          cex.lab=1.5,cex.main=1.5,cex.sub=1.5)
}
# Example:
# show_img(img_matrix(digit_array(MNIST,N=6)[,sample(1:100,1)])) # EXPL
#'
show_compared_img <- function(Data,dx,dy,x,y){
    Time <- system.time({
        layout(qm(1,3,3,3|2,3,3,3),respect = F)
        # ifelse(abs(x-y)<m,bound <- sample(m),bound <-sample(x:y,size = m))
        show_img(img_matrix(dx),x)
        show_img(img_matrix(dy),y)
        H = dist_matrix(Data)
        Hist <- hist(H,main="",
                     xlab = "Distance",ylab ='Number of Images',cex.lab=2,cex.main=2.5,cex.axis=2)
        d = distance_1D(dx,dy)
        abline(v = d, col = "blue", lwd = 2)
        text(x=d-100,y=mean(Hist$counts),labels=paste("Distance is: ",round(d,2)),pos = 2,cex=2,col= "darkblue")
    })
    mtext(paste("Show Compared Digits with Time:",round(Time[1],4)," s"),
          line = -3, cex= 1.5,col= "red",outer = T )
}
# Example:
# Data = digit_array(MNIST,N=6)[,0:20]
# show_compared_img(Data,sample(20,1),sample(20,1))
#'
process_pca <- function(X,degree=3,pca=F,pcah=F){
    c("Xbar", "mu", "std") %=% normalize(X)
    B = {if(pcah) PCA_high_dim(Xbar, degree) else
        PCA_origin(Xbar,{if(pca) degree else dim(X)[2]})} * std + mu
    return(B)
}
show_compress_img <- function(X=qm(0,0,0),Y=X,Z=X,idx=1,Time=0){
    layout(qm(1,2,3),respect = T)
    show_img(X,paste(" ",idx,"/ Origin"))
    show_img(Y,paste(" ",idx,"/ PCA"))
    show_img(Z,paste(" ",idx,"/ PCA HD"))
    # show_img(cbind(X,Y,Z))
    mtext(paste("Show PCA Compressed Digits with Time:",round(Time[1],4)," s"),
          line = -3, cex= 1.5,col= "red",outer = T )
}
# Example:
# A = img_matrix(digit_array(MNIST,0)[,1]) # Single Image
# PCA = {if(T) process_pca(A,3,T,F) else A}
# PCAH = {if(T) process_pca(A,4,F,T) else A}
# show_compress_img(A,PCA,PCAH)
#'
show_recommend_img <- function(Data,x=7,n=6,mean=F,S=0){
    Time <- system.time({
        if(S==0) S <- dist_matrix_idx(x,Data)
        idx <- order(S)[0:n]
        print(idx)
        print(S[idx,])
        A = img_matrix(Data[,idx[1]])
        B = matrix(0,28,28)
        idxB = tail(idx,-1)
        for (i in 1:length(idxB)){
            img = img_matrix(Data[,idxB[i]])
            if(mean) B <- B + img else ifelse(sum(B)==0,B <- img,B <- cbind(B,img))
        }
    })
    if(mean){
        layout(qm(1,2),respect = mean)
        show_img(A," Origin")
        show_img(B," Similar")
    }else {
        layout(1,respect = mean)
        show_img(rbind.fill.matrix(A,B),idx)
    }
    mtext(paste("Show Similar Digits with Time:",round(Time[1],4)," s"),
          line = -3, cex= 1.5,col= "red",outer = T )
    return(S)
} 
# Example:
# Data = digit_array(MNIST,N=3)[,0:50]
# show_recommend_img(Data,x=7,n=5)
#'
#'
init_page1_sample = 10
init_page1_Aidx = 50
init_page1_Bidx = 100
init_page2_PCAc = 2
init_page2_PCAHc = 8
init_page3_sample = 10
init_page3_topn = 5

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # RV = reactiveVal()  # observeEvent(input$x,RV(input$x))  # target = RV()
    #'
    v <- reactiveValues(idx1=0,idx2=0,idx3=0,idxa=init_page1_Aidx,idxb=init_page1_Bidx,
                        slider1=0,slider2=0,slider3=0,doPlot=0,go1=0,go2=0,go3=0,page3data=0)
    observeEvent(input$x1,v$idx1 <- input$x1)
    observeEvent(input$x2,v$idx2 <- input$x2)
    observeEvent(input$x3,v$idx3 <- input$x3)
    observeEvent(input$slider1,v$slider1 <- input$slider1)
    observeEvent(input$slider2,v$slider2 <- input$slider2)
    observeEvent(input$slider3,v$slider3 <- input$slider3)
    observeEvent(input$Aidx,v$idxa <- input$Aidx)
    observeEvent(input$Bidx,v$idxb <- input$Bidx)
    observeEvent(input$go1,v$go1 <- input$go1)
    observeEvent(input$go2,v$go2 <- input$go2)
    observeEvent(input$go3,v$go3 <- input$go3)
    observeEvent(c(input$go1,input$go2,input$go3),v$doPlot <- v$go1 + v$go2 + v$go3)
    observeEvent(input$tabset,{v$doPlot <-0;v$go1=0;v$go2=0;v$go3=0})
    observeEvent(c(input$x3,input$srange,input$slider3),v$page3data <- 0)
    
    
    output$sliders1 <- renderUI({
        print("1111111111111111111111")
        da = round(dim(digit_array(MNIST,N=v$idx1))[2] * v$slider1 / 100.)
        tagList(
            sliderInput('slider1','Sample Range %',
                        min=2, max=100, post=" %", value = ifelse(v$slider1,v$slider1,init_page1_sample), step=5),
            isolate({sliderInput("Aidx", "Digit A Idx",
                        min=1, max=da, value=v$idxa, step = 10)}),
            isolate({sliderInput("Bidx", "Digit B Idx",
                        min=1, max=da, value=v$idxb, step = 10)})
        )
    })
    output$sliders2 <- renderUI({
        cat("sliders2!!!")
        da = dim(digit_array(MNIST,N=v$idx2))[2]
        sliderInput("slider2", "Digit Idx",
                    min=1, max=da, value=da/2, step = 10)
    })
    output$sliders3 <- renderUI({
        cat("sliders3!!!")
        da = round(dim(digit_array(MNIST,N=v$idx3))[2] * input$srange / 100.)
        sliderInput("slider3", "Digit Idx",
                    min=1, max=da, value=ifelse(v$slider3,v$slider3,da/2), step = 10)
    })
    #'
    output$plot_img <- renderPlot({
        if(v$doPlot == F)return()
        if(v$go1 > 0){
            isolate({ 
                Data = digit_array(MNIST,input$x1)
                Data = Data[,0:round(dim(Data)[2] * input$slider1 / 100.)]
                Sample = Data[,0:input$crange]
                print(dim(Sample))
                withProgress(message = 'Calculation in progress',
                             detail = 'This may take a while...', value = 0,{
                                 show_compared_img(Sample,Data[,input$Aidx],Data[,input$Bidx],input$Aidx,input$Bidx)
                                 incProgress(1/1, detail = paste("Doing part", 1))
                                 Sys.sleep(0.01)
                             })
            })
        }
        if(v$go2 > 0){
            isolate({
                A = img_matrix(digit_array(MNIST,input$x2)[,input$slider2]) # Single Image
                withProgress(message = 'Calculation in progress',
                             detail = 'This may take a while...', value = 0,{
                                 Time <- system.time({
                                     PCA = {if(input$pcac) process_pca(A,input$pcad,T,F) else A}
                                     PCAH = {if(input$pcahc) process_pca(A,input$pcahd,F,T) else A}
                                 })
                                 show_compress_img(A,PCA,PCAH,input$slider2,Time)
                                 incProgress(1/1, detail = paste("Doing part", 1))
                                 Sys.sleep(0.01)
                             })
            })    
        }
        if(v$go3 > 0){
            isolate({
                Data = digit_array(MNIST,input$x3)
                Data = Data[,0:round(dim(Data)[2] * input$srange / 100.)] # select % of sample
                withProgress(message = 'Calculation in progress',
                             detail = 'This may take a while...', value = 0, {
                                 v$page3data = show_recommend_img(Data,x=input$slider3,n=input$topn,mean=input$mean,v$page3data)
                                 # cat("ssssssssssss",dim(v$page3data))
                                 incProgress(1/1, detail = paste("Doing part", 1))
                                 Sys.sleep(0.01)
                             })
            })
        }
    })
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Digits Data Manipulating"),
    # headerPanel("PCA Compress Numbers Data2"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2('Tuning Panel'),
            tabsetPanel(id = "tabset",
                        tabPanel("Dist Hist",
                                 selectInput('x1', 'X', 0:9,selected = 3),
                                 uiOutput("sliders1"),
                                 sliderInput('crange', 'Compare Range (Ramdon)',
                                             min = 2, max = 200, value = sample(2:200,size=1)),
                                 actionButton("go1", label = "Summit")
                        ),
                        tabPanel("PCA Compress",
                                 selectInput('x2', 'X', 0:9,selected = 6),
                                 uiOutput("sliders2"),
                                 sliderInput('pcad', 'PCA Numbers of Componet',
                                             value = init_page2_PCAc, min = 1, max = 10, step = 1),
                                 sliderInput('pcahd', 'PCA HD Numbers of Componet',
                                             value = init_page2_PCAHc, min = 1, max = 10, step = 1), 
                                 checkboxInput('pcac', 'PCA Normal',value = T),
                                 checkboxInput('pcahc', 'PCA High Dim',value = T),
                                 actionButton("go2", label = "Summit")
                        ),
                        tabPanel("Simular Digits",
                                 selectInput('x3', 'X', 0:9,selected = 9),
                                 sliderInput('srange', 'Sample Range %',
                                             min = 2, max = 100, post  = " %", value = init_page3_sample),
                                 uiOutput("sliders3"),
                                 numericInput('topn', 'Top N',
                                              min=0, max=20, value=init_page3_topn, step = 1),
                                 checkboxInput('mean', 'Show Average'),
                                 actionButton("go3", label = "Summit")
                        ))
            # textInput('itx', '', value=''),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h2('Plot Panel'),
            plotOutput("plot_img")
            # textOutput(" ")
            # plotOutput(" ")
        )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)

