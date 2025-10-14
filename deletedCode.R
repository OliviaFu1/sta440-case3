## (IMPORTANT PLEASE CHECK) AIC and BIC across LMM models

```{r}
#| label: map_plots

library(sf)
library(ggplot2)
library (dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)
library(leaflet)

ggplot(x,aes(x=REF.GPS.LON,y=REF.GPS.LAT))+geom_point(color="blue",size=3)+
  theme_minimal()+
  coord_fixed()

sf_vals<- st_as_sf(x,coords= c("REF.GPS.LON","REF.GPS.LAT"),crs=4326)

plot(sf_vals["observedTT"])

leaflet(x) |> 
  addTiles() |> 
  addCircleMarkers(~REF.GPS.LON, ~REF.GPS.LAT, popup = ~as.character(REF.GPS.LON))

```


733

```{r}
734

library(kableExtra)
735

aic_table<-AIC(lmm_no_weights, lmm_model_ext,lmm_interaction,lmm_no_dispatch_name_no_interaction,lmm_no_dispatch_name_interaction)
736

min_aic<-which.min(aic_table$AIC)
737

738

bic_table<-BIC(lmm_no_weights, lmm_model_ext,lmm_interaction,lmm_no_dispatch_name_no_interaction,lmm_no_dispatch_name_interaction)
739

min_bic<-which.min(bic_table$BIC)
740

741

742

kable(aic_table)|>kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))|>row_spec(min_aic, bold = TRUE, color = "white", background = "#1976D2")
743

kable(bic_table)|>kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))|>row_spec(min_bic, bold = TRUE, color = "white", background = "#1976D2")
744

745

746

```
747

748

749

750

751

# Evaluate benefits
752

753

## Emmeans
754

755

```{r}
756

757

library(kableExtra)
758

# overall optimal emmeans scenario
759

emmeans_overall <- emmeans(lmm_no_dispatch_name_interaction, ~ Scenario)
760

opt_overall <- as.data.frame(emmeans_overall) 
761

min<-which.min(opt_overall$emmean)
762

#slice_min(emmean, n = 1)
763

#cat("Overall optimal scenario:\n"); print(opt_overall)
764

kable(opt_overall)|>kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))|>row_spec(min, bold = TRUE, color = "white", background = "#1976D2")
765

766

# optimal scenario by region
767

emmeans_region <- emmeans(lmm_no_dispatch_name_interaction, ~ Scenario | region_category)
768

opt_by_region <- as.data.frame(emmeans_region) |>
  769

group_by(region_category) 
770

#min<-which.min(opt_by_region$emmean)
771

#slice_min(emmean, n = 1) |>
772

#ungroup()
773

#cat("\nBy region optimal scenario:\n"); print(opt_by_region)
774

kable(opt_by_region)|>kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
775

##|>row_spec(min, bold = TRUE, color = "white", background = "#1976D2")
776

```
777

778

779

## Emmeans with contrasts
780

```{r}
781

library(emmeans)
782

library(dplyr)
783

library(kableExtra)
784

785

786

# overall contrasts
787

overall_contrasts <- contrast(
  788
  
  emmeans_overall,
  789
  
  method = "trt.vs.ctrl",
  790
  
  ref = which(levels(emmeans_overall)$Scenario == "S0")  
  791
  
)
792

793

overall_contrasts_summary <- summary(overall_contrasts, infer = TRUE, adjust = "dunnett") |>
  794

as.data.frame() |>
  795

select(contrast, estimate, SE, df, t.ratio, p.value)
796

797

#cat("\nOverall contrasts (S1..S4 vs S0):\n"); print(overall_contrasts_summary)
798

kable(overall_contrasts_summary)|>kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
799

800

801

# region by region contrasts
802

region_contrasts <- contrast(
  803
  
  emmeans_region,
  804
  
  method = "trt.vs.ctrl",
  805
  
  ref = which(levels(emmeans_region)$Scenario == "S0")
  806
  
)
807

808

region_contrasts_summary <- summary(region_contrasts, infer = TRUE, adjust = "dunnett") |>
  809

as.data.frame() |>
  810

select(region_category, contrast, estimate, SE, df, t.ratio, p.value) |>
  811

arrange(region_category, contrast)
812

813

#cat("\nRegion by region contrasts (S1..S4 vs S0):\n"); print(region_contrasts_summary)
814

kable(region_contrasts_summary)|>kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
815
766
816
767
```
817
768
818
769
819

## Frequency with which the ambulance at the closest station is available
820
770
821

```{r}
822

x <- x |>
  823

mutate(
  824
  
  S0_closest_avail = (S0_ETT_DISPATCH == S0),
  825
  
  S1_closest_avail = (S1_ETT_DISPATCH == S1),
  826
  
  S2_closest_avail = (S2_ETT_DISPATCH == S2),
  827
  
  S3_closest_avail = (S3_ETT_DISPATCH == S3),
  828
  
  S4_closest_avail = (S4_ETT_DISPATCH == S4)
  829
  
)

771
# 4. Assumptions and Shortcomings\
830
772
831

closest_summary <- x |>
  832

summarise(
  833
  
  S0_freq = mean(S0_closest_avail, na.rm = TRUE),
  834
  
  S1_freq = mean(S1_closest_avail, na.rm = TRUE),
  835
  
  S2_freq = mean(S2_closest_avail, na.rm = TRUE),
  836
  
  S3_freq = mean(S3_closest_avail, na.rm = TRUE),
  837
  
  S4_freq = mean(S4_closest_avail, na.rm = TRUE)
  838
  
)

773
Normality of residuals was poorly met 

774
Large deviances at tails

775
Non-constant variance across fitted values

776
External factors not taken into consideration

777
Traffic / Rush hours/ Road closures

778
Time of year

779
EMT staffing patterns

780
ETT inputs: based on single best-guess estimates (e.g., from Google), lacking nuance or uncertainty

781
Variance modeling: only accounted for regional differences, while other factors may also influence variability

782
Emergency vs non-emergency

783
Simplicity in Dispatch Rule

784
Better for analysis

785
Doesn’t occur in real life
839
786
840

closest_summary
841
787
842

closest_by_region <- x |> 
  843

group_by(REF.GRID) |>
  844

summarise(
  845
  
  S0_freq = mean(S0_closest_avail, na.rm = TRUE),
  846
  
  S1_freq = mean(S1_closest_avail, na.rm = TRUE),
  847
  
  S2_freq = mean(S2_closest_avail, na.rm = TRUE),
  848
  
  S3_freq = mean(S3_closest_avail, na.rm = TRUE),
  849
  
  S4_freq = mean(S4_closest_avail, na.rm = TRUE)
  850
  
)

788
# 5. Conclusions
851
789
852

closest_by_region

790
Scenario 3 results in the fastest overall response times 

791
Marginal mean travel times (ETTs) are reduced by ~16% compared with the baseline (Scenario 0)
853
792
854

closest_by_priority <- x |> 
  855

group_by(DISPATCH.PRIORITY.NAME) |>
  856

summarise(
  857
  
  S0_freq = mean(S0_closest_avail, na.rm = TRUE),
  858
  
  S1_freq = mean(S1_closest_avail, na.rm = TRUE),
  859
  
  S2_freq = mean(S2_closest_avail, na.rm = TRUE),
  860
  
  S3_freq = mean(S3_closest_avail, na.rm = TRUE),
  861
  
  S4_freq = mean(S4_closest_avail, na.rm = TRUE)
  862
  
)

793
exp (−0.173) ≈ 0.84 → travel times in Scenario 3 are about 84% of baseline levels
863
794
864

closest_by_priority
865

