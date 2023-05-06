install.packages("tidyverse")
install.packages("cowplot")
library(tidyverse)
library(cowplot)

# data loading-----
assembly_data = read_csv("C:\\Users\\HABIB ADENIYI\\Downloads\\data.csv")
glimpse(assembly_data)

# make a copy of the assembly data and name it clone:
clone = data.frame(assembly_data)

#renaming the column names in clone------
colnames(clone) = c("sample_name", "assembly_result", "contamination",
                            "contamination_check_result", "percent_contamination",
                            "percent_contamination_check_result",
                            "number_of_contigs",
                            "contigs_check_result", "N50",
                            "N50_check_result", "total_length",
                            "total_length_check_result")


# Q1: to calculate the number of samples failing contamination check & have contamination----

clone %>% filter(contamination_check_result == "FAILURE" &
                           percent_contamination > 5.00) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = "", y = count))+
  geom_col(width = 0.2, fill = "blue") +
  ggtitle("number of samples that have failed contamination check & have contamination >= 5%")+
  geom_text(aes(label = count), vjust = -.5)

# Q2: to calculate the number of samples <= 50 contigs & N50 >= 750,000----
  
clone %>% filter(number_of_contigs <= 50 & N50 >= 750000) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = "", y = count))+
  geom_col(width = 0.2, fill = "blue") +
  ggtitle("number of samples with <= 50 contigs & N50 >= 750,000")+
  geom_text(aes(label = count), vjust = -.5)


# Q3: to select and rename numeric columns with prefix quast and suffix metric value----

c = assembly_data %>% 
  select(ends_with("value")) %>%
  select(-confindr.contam_status.metric_value) %>% 
  rename(contamination_percent = "confindr.percentage_contamination.metric_value",
         contig_number = "quast.# contigs (>= 1000 bp).metric_value",
         N50_value = "quast.N50.metric_value",
         total_length_estimate = "quast.Total length (>= 1000 bp).metric_value")
  
tibble(c)

# Q4: to make a boxplot of total length >= 1000bp-----

clone %>% ggplot(aes(y=total_length)) +
  geom_boxplot(fill = "green")+
  ggtitle("A boxplot of total_length >= 1000bps")


# Q5: tidying the data -----

tidy_data = assembly_data %>% pivot_longer(2:12,
                                   names_to = "metric",
                                   values_to = "value",
                                   values_transform = as.character)
tibble(tidy_data)


# Q6: to make a violin plot----

i =clone %>% ggplot(aes(x = "", y=total_length)) + geom_violin(fill = "green")+
  geom_jitter(width = 0.1)

j= clone %>% ggplot(aes(x= "", y=percent_contamination)) + geom_violin(fill = "green")+
  geom_jitter(width =0.1)

k=clone %>% ggplot(aes(x= "", y=number_of_contigs)) + geom_violin(fill = "green")+
  geom_jitter(width=0.1)

l=clone %>% ggplot(aes(x= "", y=N50)) + geom_violin(fill = "green")+
  geom_jitter(width=0.1)

plot_grid(i,j,k,l)


  
        
                                          










