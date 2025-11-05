# Load needed libraries
library("shiny")
library("bslib")
library("ggplot2")

# Load needed functions
source(file = "app_functions.R")

# Define the Server (Backend)
server <- function(input, output) {
  dna <- reactive({
    gene_dna(length = input$n_bases,
             base_probs = c(input$prob_A, 
                            input$prob_T, 
                            input$prob_C, 
                            input$prob_G))
  })
  output$dna <- renderText({
    dna()
  })
  rna <- reactive({
    transcribe_dna(dna())
  })
  output$rna <- renderText({
    rna()
  })
  protein <- reactive({
    translate_rna(rna())
  })
  output$protein <- renderText({
    protein()
  })
  output$protein_warning <- renderUI({
    if (grepl("_", protein())) {
      helpText("Underscore _ represents a STOP codon")
    }
    else {
      helpText("")
    }})
  output$plot <- renderPlot({
    base_counts <- base_freqs(dna())
    ggplot(data = base_counts,
           aes(x = dna_vec,
               y = Freq,
               fill = dna_vec)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d(option = "plasma") +
      labs(title = "Nucleotide Frequency",
           x = "Nucleotide",
           y = "Frequency") +
      theme_bw(base_size = 16) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
  }) 
}