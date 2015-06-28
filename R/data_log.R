# ------------------------------------------------------------------------------
# Lög um landhelgi, efnahagslögsögu og landgrunn

#' Grunnpunktar
#'
#' Hnit sem eru lögð til grundvallar á útreikningi á landhelgi (12 sjómílur) og
#' efnhagslögsögu Íslands sbr. 
#'  \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
#'
#' Athugið að grunnpunkar fyrir Kolbeinseyjar (67°08'9 n.br., 18°41'3 v.lg.),
#' Hvalbak (64°35'8 n.br., 13°16'6 v.lg.) og ystu annesja og skerja Grímseyjar 
#' eru grunnpunktar stórstraumsfjöruborð.
#'
#' Sjá einnig \code{\link{landhelgi}}, \code{\link{eez}}.
#' 
#' @format \code{SpatialPointsDataFrame}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
#' @source {\url{http://www.althingi.is/altext/lagas/141b/1979041.html}}
#' 
#' @examples
#' grunnpunktar@@data
#' sp::plot(iceland, col="grey90")
#' sp::plot(grunnpunktar, col="red", add=TRUE)
"grunnpunktar"

#' Grunnlína
#'
#' Línur sem eru lagðar til grundvallar á útreikningi á landhelgi (12 sjómílur) og
#' efnhagslögsögu Íslands sbr. 
#'  \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
#'
#' Línur eru byggðar á grunnpunktum, sjá \code{\link{grunnpunktar}}.
#' 
#' @format \code{SpatialPolygonsDataFrame}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @examples
#' sp::plot(iceland, col="grey90")
#' sp::plot(grunnlina, border="yellow", lwd=2, add=TRUE)
#' sp::plot(grunnpunktar, col="red", add=TRUE)
#' 
#' sp::plot(grunnlina[grunnlina$name == "Grímsey",],col="grey90")
"grunnlina"

#' Íslenska landhelgin (12 mílur)
#'
#' Landhelgi Íslands (12 mílur) reiknaðar út frá grunnlínum
#' (\code{\link{grunnlina}}) sbr.
#'  \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
#'
#' @format \code{SpatialPolygonsDataFrame}
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
#' @examples
#' sp::plot(landhelgi, col="grey90")
#' sp::plot(grunnlina, border="yellow", lwd=2, add=TRUE)
#' sp::plot(grunnpunktar, col="red", add=TRUE)
"landhelgi"

#' Íslenska landhelgin (12 mílur) - Línur
#'
#' Landhelgi Íslands (12 mílur) reiknaðar út frá grunnlínum
#' (\code{\link{grunnlina}}) sbr.
#'  \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
#'
#' @format \code{SpatialLinesDataFrame}
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
#' @examples
#' sp::plot(landhelgi_sldf, col="blue")
#' sp::plot(grunnlina, border="yellow", lwd=2, add=TRUE)
#' sp::plot(grunnpunktar, col="red", add=TRUE)
"landhelgi_sldf"


#' Íslenska efnahagslögsagan ('200 mílur')
#'
#' Íslenska efnahaglögsagan reiknuð út frá grunnlínum
#' (\code{\link{grunnlina}}) sbr.
#' \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
#' Hnit á miðlínu eru fengin úr milliríkjasamningum milli 
#' \href{http://www.althingi.is/altext/133/s/0951.html}{Íslands og Færeyjar} og milli
#' \href{http://www.althingi.is/altext/122/s/1047.html}{Íslands og Grænlands/Noregs}.
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @examples
#' sp::plot(eez,col=scales::alpha('red',0.2),border="red")
#' sp::plot(landhelgi,col=scales::alpha('red',0.2),border="red",add=TRUE)
#' sp::plot(iceland,col="grey90",add=TRUE)
"eez"

#' Íslenska efnahagslögsagan ('200 mílur') - Línur
#'
#' Íslenska efnahaglögsagan reiknuð út frá grunnlínum
#' (\code{\link{grunnlina}}) sbr.
#' \href{http://www.althingi.is/altext/lagas/141b/1979041.html}{Lög um landhelgi, efnahagslögsögu og landgrunn}
#' Hnit á miðlínu eru fengin úr milliríkjasamningum milli 
#' \href{http://www.althingi.is/altext/133/s/0951.html}{Íslands og Færeyjar} og milli
#' \href{http://www.althingi.is/altext/122/s/1047.html}{Íslands og Grænlands/Noregs}.
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @examples
#' sp::plot(eez_sldf, col="blue")
#' sp::plot(landhelgi_sldf,col="red",add=TRUE)
#' sp::plot(iceland,col="grey90",add=TRUE)
"eez_sldf"

# ------------------------------------------------------------------------------
# Lög um veiðar í fiskveiðilandhelgi Íslands

#' Viðmiðunarpunktar
#'
#' Hnit sem eru lögð til grundvallar á útreikningi á lögum um veiðar í landhelgi sbr. 
#'  \href{http://www.althingi.is/lagas/nuna/1997079.html}{Lög um veiðar í fiskveiðilandhelgi Íslands}
#' 
#' @format \code{SpatialPointsDataFrame}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
#' @source \url{http://www.althingi.is/lagas/nuna/1997079.html}
#' 
#' @examples
#' sp::plot(iceland,col="grey90")
#' sp::plot(vidmidunarpunktar, col="red", add=TRUE)
"vidmidunarpunktar"

#' Viðmiðunarlína
#'
#' Línur sem eru lagðar til grundvallar á útreikningi á lögum um veiðar í landhelgi sbr. 
#'  \href{http://www.althingi.is/lagas/nuna/1997079.html}{Lög um veiðar í fiskveiðilandhelgi Íslands}
#' 
#' Athugið að stórstraumsfjöruborð Vestmannaeyja er hér meðtalið
#' 
#' @format \code{SpatialPolygonsDataFrame}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#' 
#' @examples
#' # Fjórar sjómílur út frá viðmiðunarlínu (þ.m.t. frá Vestmannaeyjum)
#' sp::plot(expand_sp(vidmidunarlina, miles=4), col=scales::alpha('red',0.2),lwd=0.001,border='red')
#' sp::plot(iceland, col="grey90", add=TRUE)
#' sp::plot(vidmidunarlina, border="yellow", lwd=2, add=TRUE)
#' sp::plot(vidmidunarpunktar, col="red", add=TRUE)
"vidmidunarlina"

#' Heimildir fiskiskipa til veiða í 1. flokki
#' 
#' Skv. \href{http://www.althingi.is/altext/stjt/1997.079.html}{Lögum um veiðar í fiskveiðilandhelgi Íslands}
#' miðast heimildir fiskiskipa til veiða með botnvörpu, flotvörpu og dragnót innan 
#' fiskveiðilandhelgi Íslands stærðir skipa og aflvísa þeirra.
#' 
#' 1. flokkur: Fiskiskip 42 metrar og lengri. Enn fremur öll fiskiskip með aflvísa 2.500 eða hærri.
#' 
#' @format \code{SpatialPolygons}
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
"skipaflokkur1"

#' Heimildir fiskiskipa til veiða í 2. flokki
#' 
#' Skv. \href{http://www.althingi.is/altext/stjt/1997.079.html}{Lögum um veiðar í fiskveiðilandhelgi Íslands}
#' miðast heimildir fiskiskipa til veiða með botnvörpu, flotvörpu og dragnót innan 
#' fiskveiðilandhelgi Íslands stærðir skipa og aflvísa þeirra.
#' 
#' 2. flokkur: Fiskiskip lengri en 29 metrar en styttri en 42 metrar með aflvísa 
#' lægri en 2.500. Enn fremur fiskiskip styttri en 29 metrar en með aflvísa 
#' 1.600 og hærri. Í þennan flokk falla einnig fiskiskip 39 metrar og styttri 
#' sem togveiðiheimildir höfðu eftir þeirri viðmiðun skv. 
#' \href{http://www.althingi.is/lagas/nuna/1976081.html}{3. gr. laga nr. 81 31. maí 1976},
#' um veiðar í fiskveiðilandhelgi Íslands, enda verði ekki um 
#' aukningu á aflvísum þeirra að ræða eftir 1. júní 1997.
#' 
#' @format \code{SpatialPolygons}
#'
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
"skipaflokkur2"

#' Heimildir fiskiskipa til veiða í 2. flokki
#' 
#' Skv. \href{http://www.althingi.is/altext/stjt/1997.079.html}{Lögum um veiðar í fiskveiðilandhelgi Íslands}
#' miðast heimildir fiskiskipa til veiða með botnvörpu, flotvörpu og dragnót innan 
#' fiskveiðilandhelgi Íslands stærðir skipa og aflvísa þeirra.
#' 
#' 3. flokkur: Fiskiskip styttri en 29 metrar, enda séu þau með lægri aflvísa en 
#' 1.600. Enn fremur fiskiskip 26 metrar og styttri sem togveiðiheimildir höfðu 
#' eftir þeirri viðmiðun skv.
#' \href{http://www.althingi.is/lagas/nuna/1976081.html}{3. gr. laga nr. 81 31. maí 1976},
#' um veiðar í fiskveiðilandhelgi Íslands, enda verði ekki um aukningu á aflvísum 
#' þeirra að ræða eftir 1. júní 1997. 
#' 
#' @format \code{SpatialPolygons}
#' 
#' @author Einar Hjorleifsson <einar.hjorleifsson@@gmail.com>
#'
"skipaflokkur3"
