# SOCR Clustering Calculator: Latest Edition

## Overview

The SOCR Clustering Calculator is a comprehensive online tool designed to facilitate clustering calculations, output clustering results, generate plots, and perform ANOVA pairwise Comparison. It's tailored for both beginners and advanced users in data analysis.

**For Future Developers:** Please refer to potential future improvement section for some future development directions.

## Clustering Techniques

This versatile application encompasses a variety of clustering algorithms such as K-means, Gaussian Mixture, Spectral Clustering, Fuzzy C-Means, Decision Tree, Hierarchical Clustering, and Poisson Clustering. Key features include:

- Plot generation for visual interpretation.
- Downloading results in CSV format.
- Exporting plots in HTML format.

**Important Note:** Each clustering algorithm is compatible with specific data formats. In case of a clustering failure, refer to the error message for guidance on dataset requirements.

## ANOVA Pairwise Comparison

The ANOVA Pairwise Comparison feature extends beyond just conducting ANOVA tests on datasets. It's capable of performing Post-Hoc Tests through pairwise p-value comparisons and generating informative plots. This functionality draws its inspiration from the [moreThanANOVA](https://github.com/womeimingzi11/moreThanANOVA) GitHub repository. There are many functionalities in this section including one-way ANOVA test and Kruskal Wallis H test.

**Important Notes:** This feature is currently designed to support datasets with two columns: the first for group/category designation and the second for corresponding values. We encourage future developers to enhance this tool to accommodate various dataset formats.

In terms of functionality, the ANOVA Pairwise Comparison mirrors the computational logic of moreThanANOVA but with significant enhancements. A notable addition is the ability to produce an interactive plot showcasing the violin plots and Post-Hoc Test results. Users can expect to see two types of plots:
1. A standard plot displaying the Post-Hoc test outcomes.
2. An interactive plot integrating both a violin plot and Post-Hoc test results, with the pairwise p-value comparison details accessible via the "Interactive Plot" button.

## 1-way ANOVA Computation ##

This function can run only the 1-way ANOVA testing for the provided dataset. It cannot generate the plot. However, this function can allow two types of dataset:
1. A two-column format, where the first column contains the data values and the second column contains the corresponding group labels ('data', 'group')
2. A multi-column format, where each column represents a different group, and the rows contain the data values for each group ('group1', 'group2', 'group3', ...)

## Clustered Results Downloading ##

In addition to the download capabilities for various clustering algorithms, we have introduced a "Combined Clustered Results" feature. This innovative function allows users to download an HTML file encompassing all the interactive plots from the selected clustering algorithms. The current implementation of this feature utilizes HTML, JavaScript, and CSS for robust and interactive output. We encourage future developers to explore the integration of R Markdown (RMD) templates as an alternative approach for enhancing this functionality.

**Current Progress of R Markdown templates** 

Our approach to data transfer in rmd files involves exporting data to the global environment during application runtime. Subsequently, this data is directly utilized in the template.rmd. Towards the end of the template.rmd, there is code designed to encapsulate the data in the global environment into an Rdata file, ensuring that the data persists for future imports. This methodology safeguards against the loss of data in the global environment when RStudio is closed.

Code in server.r
```
assign("data_input_plot_tree", correct, envir = .GlobalEnv)
```

Insert this code line where the image is generated. You have the option to establish a dedicated page in ui.r for centralized management or position it at the conclusion of each clustering process. The string in the first spot is employed to specify the local name, while the final "envir" parameter is utilized to define the environment. This facilitates the export of data to the global environment.

Code in template.rmd

At present, only a handful of straightforward code snippets are available for clustering plots. It has been verified that KMeans, Fuzzy, and Hierarchical methods are functional. However, there are unresolved bugs in other aspects. If the dataset includes X3, it corresponds to a 3D image; otherwise, it is considered a 2D image.

The final code block serves as an illustration of data export. Before executing, ensure the confirmation of the export path, followed by data packaging. It's worth noting that there's no necessity to package a solitary data frame; an Rdata file is capable of encompassing multiple data frames.

## Potential Future Improvement ##

This section introduces few parts that can be improved in the future

### Pairwise ANOVA Comparison ###
1. **Enhanced Sorting in Pairwise Comparison:** The present version displays all pairwise comparisons of P-values for groups in the dataset. A beneficial enhancement would be to sort these outputs based on the P-values. We propose to include an option allowing users to select between the most similar or most different pairs. Based on this selection, the system could then provide a sorted list of pairwise results. Furthermore, it's feasible to add a feature where users can choose to view either the top 5 pairs with the highest P-values or the top 5 with the lowest, tailoring the output to their specific needs.
2. **Optimizing Interactive Plot Logic:** The interactive plot currently relies on data extracted from the output objects of ggbetweenstats and pairwise_comparisons. We encourage future developers to review the existing code to grasp the underlying logic of this data extraction process. There's potential to streamline and optimize this feature for more efficient and effective data handling.
3. **Streamlining Plot Generation:** In the current setup, users need to initiate the stable plot by clicking the 'Start Plot' button, followed by activating the interactive plot separately. A more user-friendly approach would be to consolidate this into a single action, perhaps with one button that simultaneously generates both the stable and interactive plots. This modification would simplify the user experience and enhance overall workflow efficiency.
4. **Supporting different dataset format:** In the current setup, this function only support two columns dataset. We encourage future developers to make this to support different dataset format. Please refer the code in 1-way ANOVA Computation because this section can support mutiple columns dataset.

### 1-way ANOVA Computation:
1. This function can be removed if the Pairwise ANOVA Comparison can support different format of dataset since they are doing the same work.

### Clustered Results Downloading:
1. This function can work well by outputing an HTML file. We encourage to make the output HTML file more beautiful by adding Javascript or CSS code to it.
2. Using Rmd template is another way to generate the plot. The future developers can explore this new way.

### Decision Tree Clustering:
1. Decision Tree Clustering method doesn't have a proper HTML downloading function for now. The future developers should review the code and add a interactive plot downloading function for this.

# Credits
Authors: Yanghe Liu, [SOCR Team](https://www.socr.umich.edu/people/).
