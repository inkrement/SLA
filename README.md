Service Level Agreements
===

This folder contains the creation and reflection of an sla-generation for sample customers. We fetched sample data from the failure trace archive to model users (you will find the specific scripts in the skype, microsoft and data_websites-subfolders). The main "logic" (clustering work, data preparation, normalization, etc.) can be found in the `sla.R` script. For the slides take a look at the `Slides.pdf` file, which contains the presentation slides.

How to run script
---

Install R (ubuntu: sudo apt-get install r-base) and run the following command:

> Rscript sla.R 
