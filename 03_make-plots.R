library(hrbrthemes)
library(ggplot2)
library(tidyverse)

# -------------
# Overall plots
# -------------

drugsOverall <- drugOverall |>
  filter(suppression == 'F') |>
    ggplot() +
      geom_col(aes(x = totalRX, y = reorder(as.factor(quarter), desc(quarter)), fill = quarter), color = 'black') +
      scale_x_continuous(labels = scales::comma) +
      theme_ipsum_rc() +
      ggtitle("Total Prescriptions by Quarter") +
      xlab("Total Prescriptions") +
      ylab("Quarter") +
      scale_fill_viridis_c()

drugsOverallGeneric <- drugOverallGeneric |>
  filter(suppression == 'F') |>
    ggplot() +
      geom_col(aes(x = totalRX, y = reorder(as.factor(quarter), desc(quarter)), fill = gennme), color = 'black') +
      facet_wrap(~gennme) +
      theme_ipsum_rc() +
      xlab("Total Prescriptions") +
      ylab("Quarter") +
      ggtitle("Total Prescriptions by Generic Name") +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 90)
      ) +
      scale_x_continuous(labels = scales::comma) +
      scale_fill_viridis_d()

drugsOverallGenericBrand <- drugOverallGenericBrand |> 
  filter(suppression == 'F') |>
    ggplot() +
      geom_col(aes(x = totalRX, y = reorder(as.factor(quarter), desc(quarter)), fill = prodnme), color = 'black') +
      facet_wrap(~prodnme) +
      theme_ipsum_rc() +
      xlab("Total Prescriptions") +
      ylab("Quarter") +
      ggtitle("Total Prescriptions by Brand Name") +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 90)
        ) +
      scale_x_continuous(labels = scales::comma) +
      scale_fill_viridis_d()

overallDrugsByState <- drugAggState |>
  filter(suppression == 'F') |>
    ggplot() +
      geom_col(aes(x = totalRX, y = reorder(as.factor(quarter), desc(quarter)), fill = quarter), color = 'black') +
      facet_wrap(~state) +
      scale_x_continuous(labels = scales::comma) + 
      scale_fill_viridis_c() +
      theme_ipsum_rc() +
      xlab("Total Prescriptions") +
      ylab("Quarter") +
      ggtitle("Total Prescriptions by State") +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 90)
      )

Cairo::CairoPDF(onefile = TRUE, file = paste0("./plots/", format(Sys.Date(), "%Y%m%d"), "_overall-plots"), width = 20, height = 16)
drugsOverall
drugsOverallGeneric
drugsOverallGenericBrand
overallDrugsByState
dev.off()

# -----------
# State plots
# -----------

states <- unique(drugAggState$state)

plotDrugsGeneric <- function(st) {
  ggplot(data = drugAggStateGeneric[drugAggStateGeneric$state == st & drugAggStateGeneric$suppression == 'F', ]) +
    geom_col(aes(x = totalRX, y = reorder(as.factor(quarter), desc(quarter)), fill = gennme), color = 'black') +
    facet_wrap(~gennme) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_viridis_d() +
    theme_ipsum_rc() +
    ggtitle(paste0("Total Prescriptions by Generic Name, State: ", st)) +
    xlab("Total Prescriptions") +
    ylab("Quarter") +
    theme(
      legend.position = 'none',
      axis.text.x = element_text(angle = 90)
    )
}

listOfGenericPlots <- map(
  .x = states,
  .f = ~plotDrugsGeneric(.x)
)

Cairo::CairoPDF(onefile = TRUE, file = paste0("./plots/", format(Sys.Date(), "%Y%m%d"), "_state-generic-plots.pdf"), width = 30, height = 22)
listOfGenericPlots
dev.off()

plotDrugsBrand <- function(st) {
  ggplot(data = drugAggStateGenericBrand[drugAggStateGenericBrand$state == st & drugAggStateGenericBrand$suppression == 'F', ]) +
    geom_col(aes(x = totalRX, y = reorder(as.factor(quarter), desc(quarter)), fill = prodnme), color = 'black') +
    facet_wrap(~prodnme) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_viridis_d() +
    theme_ipsum_rc() +
    ggtitle(paste0("Total Prescriptions by Brand Name, State: ", st)) +
    theme(
      legend.position = 'none',
      axis.text.x = element_text(angle = 90)
      ) +
    xlab("Total Prescriptions") + 
    ylab("Quarter") 
}

listOfPlotsBrand <- map(
  .x = states,
  .f = ~plotDrugsBrand(.x)
)

Cairo::CairoPDF(onefile = TRUE, file = paste0("./plots/", format(Sys.Date(), "%Y%m%d"), "_state-brand-plots.pdf"), width = 30, height = 22)
listOfPlotsBrand
dev.off()
