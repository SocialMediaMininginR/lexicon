################
## data prep ###
################
dallas <- subset(BB.sentiment, location=="Dallas")
stlouis <- subset(BB.sentiment, location=="St. Louis")
atlanta <- subset(BB.sentiment, location=="Atlanta")
ny <- subset(BB.sentiment, location=="New York")
richmond <- subset(BB.sentiment, location=="Richmond")
sf <- subset(BB.sentiment, location=="San Francisco")
kc <- subset(BB.sentiment, location=="Kansas City")
minneapolis <- subset(BB.sentiment, location=="Minneapolis")
chicago <- subset(BB.sentiment, location=="Chicago")
boston <- subset(BB.sentiment, location=="Boston")
cleveland <- subset(BB.sentiment, location=="Cleveland")
phili <- subset(BB.sentiment, location=="Philadelphia")

################
## four plots (dallas, stlouis, ny, atlanta)
################
png(filename="/Users/heimannrichard/Google Drive/Spatial Analysis UMBC/RCode/casestudy1/dsna.png")
# bb.boxplot.dallas 
bb.boxplot.dallas <- ggplot(dallas, aes(x=dallas$year, y=dallas$centered, group=dallas$year))
bb.boxplot.dallas <- bb.boxplot.dallas + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-70, 50)
bb.boxplot.dallas <- bb.boxplot.dallas + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("dallas")
# BB.boxplot.stlouis 
BB.boxplot.stlouis <- ggplot(stlouis, aes(x=stlouis$year, y=stlouis$centered, group=stlouis$year))
BB.boxplot.stlouis <- BB.boxplot.stlouis + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.stlouis <- BB.boxplot.stlouis + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("stlouis")
# BB.boxplot.atlanta 
BB.boxplot.atlanta <- ggplot(atlanta, aes(x=atlanta$year, y=atlanta$centered, group=atlanta$year))
BB.boxplot.atlanta <- BB.boxplot.atlanta + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.atlanta <- BB.boxplot.atlanta + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("atlanta")
# BB.boxplot.ny 
BB.boxplot.ny <- ggplot(ny, aes(x=ny$year, y=ny$centered, group=ny$year))
BB.boxplot.ny <- BB.boxplot.ny + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.ny <- BB.boxplot.ny + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("ny")
multiplot(bb.boxplot.dallas, BB.boxplot.stlouis, BB.boxplot.atlanta, BB.boxplot.ny, cols=2) #BB.boxplot.atlanta
dev.off()

################
## four plots (richmond, sf, kc, minneapolis)
################
png(filename="/Users/heimannrichard/Google Drive/Spatial Analysis UMBC/RCode/casestudy1/rskm.png")
# bb.boxplot.richmond 
bb.boxplot.richmond <- ggplot(richmond, aes(x=richmond$year, y=richmond$centered, group=richmond$year))
bb.boxplot.richmond <- bb.boxplot.richmond + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-70, 50)
bb.boxplot.richmond <- bb.boxplot.richmond + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("richmond")
# BB.boxplot.sf 
BB.boxplot.sf <- ggplot(sf, aes(x=sf$year, y=sf$centered, group=sf$year))
BB.boxplot.sf <- BB.boxplot.sf + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.sf <- BB.boxplot.sf + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("sf")
# BB.boxplot.kc 
BB.boxplot.kc <- ggplot(kc, aes(x=kc$year, y=kc$centered, group=kc$year))
BB.boxplot.kc <- BB.boxplot.kc + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.kc <- BB.boxplot.kc + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("kc")
# BB.boxplot.minneapolis
BB.boxplot.minneapolis <- ggplot(minneapolis, aes(x=minneapolis$year, y=minneapolis$centered, group=minneapolis$year))
BB.boxplot.minneapolis <- BB.boxplot.minneapolis + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.minneapolis <- BB.boxplot.minneapolis + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("minneapolis")
multiplot(bb.boxplot.richmond, BB.boxplot.sf, BB.boxplot.kc, BB.boxplot.minneapolis, cols=2) #BB.boxplot.atlanta
dev.off()

################
## four plots (chicago, boston, cleveland, phili)
################
png(filename="/Users/heimannrichard/Google Drive/Spatial Analysis UMBC/RCode/casestudy1/cbcp.png")
# bb.boxplot.chicago 
bb.boxplot.chicago <- ggplot(chicago, aes(x=chicago$year, y=chicago$centered, group=chicago$year))
bb.boxplot.chicago <- bb.boxplot.chicago + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-70, 50)
bb.boxplot.chicago <- bb.boxplot.chicago + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("chicago")
# BB.boxplot.boston 
BB.boxplot.boston <- ggplot(boston, aes(x=boston$year, y=boston$centered, group=boston$year))
BB.boxplot.boston <- BB.boxplot.boston + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.boston <- BB.boxplot.boston + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("boston")
# BB.boxplot.cleveland 
BB.boxplot.cleveland <- ggplot(cleveland, aes(x=cleveland$year, y=cleveland$centered, group=cleveland$year))
BB.boxplot.cleveland <- BB.boxplot.cleveland + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.cleveland <- BB.boxplot.cleveland + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("cleveland")
# BB.boxplot.phili 
BB.boxplot.phili <- ggplot(phili, aes(x=phili$year, y=phili$centered, group=phili$year))
BB.boxplot.phili <- BB.boxplot.phili + geom_boxplot(outlier.colour = "black", outlier.shape = 16,  outlier.size = 2) + ylim(-50, 50)
BB.boxplot.phili <- BB.boxplot.phili + xlab("Date") + ylab("Sentiment (Centered)") + ggtitle("phili")
multiplot(bb.boxplot.chicago, BB.boxplot.boston, BB.boxplot.cleveland, BB.boxplot.phili, cols=2) #BB.boxplot.atlanta
dev.off()