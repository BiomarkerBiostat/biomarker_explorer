#-----------------------------------------------------------------------------
# Purpose:  Function to draw KM-curve
# Author:   Feiyang Niu
# Date:     November 1, 2016
#-----------------------------------------------------------------------------


# load required r scripts
install_('survival')


km_plot <- function(survdata, tte, cens, strata, strata.labels = NULL,
                    plot.nrisk=TRUE, nrisk.interval=2,
                    nrisk.plotheight,cex.nrisk=1,
                    plot.CI=FALSE, plot.medsurv=TRUE,
                    col=1:length(levels(survdata[,strata])), lty=1, lwd=2,
                    plot.grid=TRUE, grids=seq(0,1,0.1), plot.legend=TRUE,
                    xlim = NULL, ylim=c(0,1),
                    xlab="Months To Event Or Censoring (PFS)",
                    ylab="Survival Probability", main="",sub="",
                    mar = NULL,plot.pdf=FALSE,pdfname="KMplot") {
    if(!is.factor(survdata[[strata]]))
        survdata[[strata]] <- factor(survdata[[strata]])
    strat.vec <- survdata[[strata]]
    if(missing(strata.labels))
        strata.labels <- levels(strat.vec)
    fit <- survival::survfit(
        as.formula(paste("survival::Surv(",tte,",",cens,") ~ ",strata)),
        data=survdata
    )    
    if(is.null(xlim)){
        xlim2 <- max(survdata[,tte],na.rm=TRUE)
        if(plot.nrisk)
            xlim1 <- -0.5
        else
            xlim1 <- 0
        xlim <- c(xlim1, xlim2)
    }
    if(is.null(mar)) {
        main_lines <- if(is_blank(main)) 0 else length(strsplit(main, '\n')[[1]])
        mar <- c(7, 6, main_lines * 1.2 + 1, 1)
    }
    if(plot.nrisk) {
        time.pt <- seq(0,xlim[2],nrisk.interval)
        ix = 0
        n.risk = c()
        for (kk in 1:(length(fit$strata)))
        {
            fit.n.risk = fit$n.risk[(ix+1) : (ix+fit$strata[kk])]
            fit.time = fit$time[(ix+1) : (ix+fit$strata[kk])]
            tmp = findInterval(time.pt, fit.time)
            n.risk=rbind(n.risk, ifelse(tmp<length(fit.time), fit.n.risk[tmp+1], 0))
            ix = ix + fit$strata[kk]
        }
        dimnames(n.risk)[[2]] = time.pt
    }
    if(plot.nrisk){
        #if(missing(nrisk.plotheight))
        #  nrisk.plotheight <- length(levels(strata))*0.3
        #layout(matrix(1:2, ncol=1), height=c(5, nrisk.plotheight))
        if(mar[1]<4+length(levels(strat.vec))) mar[1] <- 4+length(levels(strat.vec))
    }
    if(plot.pdf)
        pdf(pdfname, width=7, height=6)
    par(xaxs="r",mar=mar)
    #   if(plot.CI){
    #     CI.lty <- c(1,2,2)
    #     CI.lwd <- c(lwd,1,1)
    #   }
    #   else{
    CI.lty <- lty
    CI.lwd <- lwd
    # }
    plot(fit,conf.int=plot.CI,xlab="", ylab=ylab,
         lty=CI.lty,col=col,lwd=CI.lwd,main=main, sub=sub, 
         axes=FALSE, ylim=ylim, xlim=xlim); box()
    med.surv <- round(summary(fit)$table[,"median"],2)
    med.surv.text <- paste(strata.labels, " MST: ", med.surv,sep="")
    mtext(xlab,side=1, line=2)
    if(plot.legend)
        legend("topright",strata.labels, lwd=lwd[1], col=col, lty=lty, bty="n")
    axis(1,at=seq(0,xlim[2],nrisk.interval),seq(0,xlim[2],nrisk.interval))
    axis(2,at=seq(ylim[1],ylim[2],0.1), seq(ylim[1],ylim[2],0.1),las=2); abline(h=0)
    if(plot.grid) abline(h=grids, col="gray")
    if(plot.nrisk){
        mtext(side=1, at=xlim[1]-0.00*(xlim[2]-xlim[1]), line=4,text='N =',
              adj=1,cex=0.8)
        for(i in 1:length(levels(strat.vec))){
            mtext(side=1, at=time.pt, line=i+3,text=n.risk[i,],col=col[i],cex=0.8)
        }
        
        #par(xaxs="r",mar=c(0.5,mar[2],0.5,mar[4]))
        #plot(NA, xlim=xlim, ylim=c(0,1), type="n",xlab="",ylab="",axes=FALSE)
        #mtext(side=2,at=seq(0.1,1-0.1,length=length(levels(strata))),
        #      strata.labels,las=2,cex.axis=0.6,col=col)
        #text(rep(time.pt,length(levels(strata))),
        #     rep(seq(0.1,1-0.1,length=length(levels(strata))),
        #                         each=length(time.pt)), t(n.risk),cex=cex.nrisk)
        
    }
    
    if(plot.medsurv)
    {
        text(x=xlim[1]+.5, y=ylim[1]+.05+.1*c(1:length(med.surv.text)-1), med.surv.text, srt = 0, 
             pos = 4, xpd = TRUE, offset=0, cex=1)
    }
    
    #if(plot.pdf)
    #dev.off()
}


