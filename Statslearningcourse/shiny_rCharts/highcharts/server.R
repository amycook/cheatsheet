
shinyServer(function(input, output) {
        output$chart <- renderChart({
                a <- rHighcharts:::Chart$new()
                a$title(text = "Fruits")
                a$data(x = c("Apples","Bananas","Oranges"), y = c(15, 20, 30), type = "pie", name = "Amount")
                return(a)
        })
})