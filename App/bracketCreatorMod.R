require(data.table)
require(lpSolve)
require(tidyr)

bracketCreatorUI <- function(id){
  tagList(
    br(),
    div(
      plotOutput(outputId = NS(id,"bracket"))
    ),
    br()
  ) # close taglist
}

bracketCreatorServer <- function(id, ps){
  moduleServer(
    id,
    function(input,output,session){

      x1_len = 1
      y1_len = 1

      output$bracket <- renderPlot({
        ps |> ggplot(aes(x, y, label = paste0(conf_rank,": ",team))) +
          scale_x_continuous(limits = c(-1, 13)) +
          scale_y_continuous(limits = c(-1, 14)) +

          # separate AFC and NFC sections
          # annotate("rect", xmin = -1, xmax = 6, ymin = -1, ymax = 14, alpha = .2, fill = "red") +
          # annotate("rect", xmin = 6, xmax = 13, ymin = -1, ymax = 14, alpha = .2, fill = "blue") +

          # AFC and NFC headers
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+3,
                   y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len*2,
                   label = "AFC") +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-3,
                   y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len*2,
                   label = "NFC") +

          # Week subheaders
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*0,
                   y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 1") +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 2") +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
                   y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 3") +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*6,
                   y = ps[conf=="AFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 4") +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len*0,
                   y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 1") +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len*-2,
                   y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 2") +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len*-4,
                   y = ps[conf=="NFC" & conf_rank==1L]$y+y1_len,
                   label = "Week 3") +

          # week 1, byes
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x-x1_len,
                   y = ps[conf=="AFC" & conf_rank==1L]$y,
                   label = "Bye") +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x+x1_len,
                   y = ps[conf=="NFC" & conf_rank==1L]$y,
                   label = "Bye") +

          # week 1, rank 1 segment; no vertical connector because of the bye
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==1L]$x,
                   xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==1L]$y,
                   yend = ps[conf=="AFC" & conf_rank==1L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==1L]$x,
                   xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
                   y = ps[conf=="NFC" & conf_rank==1L]$y,
                   yend = ps[conf=="NFC" & conf_rank==1L]$y) +

          # AFC week 1, rank 2 and 7 bracket
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==2L]$x,
                   xend = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==2L]$y,
                   yend = ps[conf=="AFC" & conf_rank==2L]$y) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==7L]$x,
                   xend = ps[conf=="AFC" & conf_rank==7L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==7L]$y,
                   yend = ps[conf=="AFC" & conf_rank==7L]$y) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
                   xend = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==2L]$y,
                   yend = ps[conf=="AFC" & conf_rank==2L]$y-y1_len) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==2L]$x+x1_len,
                   xend = ps[conf=="AFC" & conf_rank==2L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==2L]$y-y1_len*0.5,
                   yend = ps[conf=="AFC" & conf_rank==2L]$y-y1_len*0.5) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==2L]$x+x1_len*1.5,
                   y = ps[conf=="AFC" & conf_rank==2L]$y-y1_len*0,
                   label = "BUF") +

          # NFC week 1, rank 2 and 7 bracket
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==2L]$x,
                   xend = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==2L]$y,
                   yend = ps[conf=="NFC" & conf_rank==2L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==7L]$x,
                   xend = ps[conf=="NFC" & conf_rank==7L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==7L]$y,
                   yend = ps[conf=="NFC" & conf_rank==7L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
                   xend = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==2L]$y,
                   yend = ps[conf=="NFC" & conf_rank==2L]$y-y1_len) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==2L]$x-x1_len,
                   xend = ps[conf=="NFC" & conf_rank==2L]$x-x1_len*2,
                   y = ps[conf=="NFC" & conf_rank==2L]$y-y1_len*0.5,
                   yend = ps[conf=="NFC" & conf_rank==2L]$y-y1_len*0.5) +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==2L]$x-x1_len*1.5,
                   y = ps[conf=="NFC" & conf_rank==2L]$y-y1_len*0,
                   label = "PHI") +

          # AFC week 1, rank 3 and 6 bracket
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==3L]$x,
                   xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==3L]$y,
                   yend = ps[conf=="AFC" & conf_rank==3L]$y) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==6L]$x,
                   xend = ps[conf=="AFC" & conf_rank==6L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==6L]$y,
                   yend = ps[conf=="AFC" & conf_rank==6L]$y) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
                   xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==3L]$y,
                   yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len,
                   xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0.5,
                   yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0.5) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*1.5,
                   y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0,
                   label = "BAL") +

          # NFC week 1, rank 3 and 6 bracket
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==3L]$x,
                   xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==3L]$y,
                   yend = ps[conf=="NFC" & conf_rank==3L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==6L]$x,
                   xend = ps[conf=="NFC" & conf_rank==6L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==6L]$y,
                   yend = ps[conf=="NFC" & conf_rank==6L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
                   xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==3L]$y,
                   yend = ps[conf=="NFC" & conf_rank==3L]$y-y1_len) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len,
                   xend = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*2,
                   y = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0.5,
                   yend = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0.5) +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==3L]$x-x1_len*1.5,
                   y = ps[conf=="NFC" & conf_rank==3L]$y-y1_len*0,
                   label = "WAS") +

          # AFC week 1, rank 4 and 5 bracket
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==4L]$x,
                   xend = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==4L]$y,
                   yend = ps[conf=="AFC" & conf_rank==4L]$y) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==5L]$x,
                   xend = ps[conf=="AFC" & conf_rank==5L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==5L]$y,
                   yend = ps[conf=="AFC" & conf_rank==5L]$y) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
                   xend = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
                   y = ps[conf=="AFC" & conf_rank==4L]$y,
                   yend = ps[conf=="AFC" & conf_rank==4L]$y-y1_len) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==4L]$x+x1_len,
                   xend = ps[conf=="AFC" & conf_rank==4L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==4L]$y-y1_len*0.5,
                   yend = ps[conf=="AFC" & conf_rank==4L]$y-y1_len*0.5) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==4L]$x+x1_len*1.5,
                   y = ps[conf=="AFC" & conf_rank==4L]$y-y1_len*0,
                   label = "HOU") +

          # NFC week 1, rank 4 and 5 bracket
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==4L]$x,
                   xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==4L]$y,
                   yend = ps[conf=="NFC" & conf_rank==4L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==5L]$x,
                   xend = ps[conf=="NFC" & conf_rank==5L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==5L]$y,
                   yend = ps[conf=="NFC" & conf_rank==5L]$y) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
                   xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
                   y = ps[conf=="NFC" & conf_rank==4L]$y,
                   yend = ps[conf=="NFC" & conf_rank==4L]$y-y1_len) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len,
                   xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*2,
                   y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0.5,
                   yend = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0.5) +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*1.5,
                   y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0,
                   label = "LA") +

          # AFC week 2, rank 1 to rank 4/5 extensions
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
                   xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==1L]$y,
                   yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*2.5) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*2,
                   xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
                   y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*1.25,
                   yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*1.25) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*3,
                   y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*0.75,
                   label = "KC") +

          # NFC week 2, rank 1 to rank 4/5 extensions
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
                   xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
                   y = ps[conf=="NFC" & conf_rank==1L]$y,
                   yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*2.5) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*2,
                   xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
                   y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*1.25,
                   yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*1.25) +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*3,
                   y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*0.75,
                   label = "WAS") +

          # AFC week 2, rank 3/6 to rank 2/7 extensions
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
                   xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
                   y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*0.5,
                   yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*3.5) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*2,
                   xend = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*4,
                   y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*2,
                   yend = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*2) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==3L]$x+x1_len*3,
                   y = ps[conf=="AFC" & conf_rank==3L]$y-y1_len*1.5,
                   label = "BUF") +

          # NFC week 2, rank 4/5 to rank 2/7 extensions
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*2,
                   xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*2,
                   y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*0.5,
                   yend = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*3.5) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*2,
                   xend = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*4,
                   y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*2,
                   yend = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*2) +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==4L]$x-x1_len*3,
                   y = ps[conf=="NFC" & conf_rank==4L]$y-y1_len*1.5,
                   label = "PHI") +

          # AFC week 3, rank 1/4/5 to rank 2/7/3/6 extensions
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
                   xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
                   y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*1.25,
                   yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*7) +
          annotate("segment",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4,
                   xend = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*5.5,
                   y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4,
                   yend = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*4.75,
                   y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*3.5,
                   label = "KC") +

          # NFC week 3, rank 1/3/6 to rank 2/7/4/5 extensions
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
                   xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
                   y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*1.25,
                   yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*7) +
          annotate("segment",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4,
                   xend = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*5.5,
                   y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*4,
                   yend = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*4) +
          annotate("text",
                   x = ps[conf=="NFC" & conf_rank==1L]$x-x1_len*4.75,
                   y = ps[conf=="NFC" & conf_rank==1L]$y-y1_len*3.5,
                   label = "PHI") +

          # Superbowl
          annotate("rect",
                   xmin = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*5.5,
                   xmax = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*6.5,
                   ymin = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4.5,
                   ymax = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*3.5,
                   alpha = 0.2) +
          annotate("text",
                   x = ps[conf=="AFC" & conf_rank==1L]$x+x1_len*6,
                   y = ps[conf=="AFC" & conf_rank==1L]$y-y1_len*4,
                   label = "TBD") +




          geom_label() +
          theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill = "white", color = "black")
          )
      })

    }
  ) #close moduleServer
}
