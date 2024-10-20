# Project SafeTainAble

A predictive analytics project designed to enhance safety and improve risk management for Aramco’s oil industry operations by forecasting pipeline accident costs and classifying high-cost incidents.

**Team**: Celeste Ang Jianing, Lim Kiat Sen Jaron, Sally Ngui Yu Ying, Wu Rixin, Zhang Xinyang

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)

### Documents

- [Report](https://github.com/thenewgoat/Project-SafeTainAble/blob/main/BC2406_S04_Grp1.docx)
- [Slides](https://github.com/thenewgoat/Project-SafeTainAble/blob/main/BC2406_S04_Grp1%20Presentation%20Slides.pptx)

## Executive Summary

In the high-stakes environment of the oil industry, ensuring the safety of operations is crucial for protecting workers, the environment, and sustaining long-term business viability. Aramco, a leader in the global oil sector, is focused on proactively managing risks rather than reacting to incidents as they occur. 

Project SafeTainAble addresses this need by developing predictive models that estimate the cost of pipeline accidents and classify whether incidents are likely to be high-cost. The project utilizes data sourced from the U.S. Pipeline and Hazardous Materials Safety Administration (PHMSA) due to the unavailability of Saudi-specific data. This dataset was cleaned and processed to prepare it for predictive modeling.

Three models—Linear Regression, CART, and Logistic Regression—were developed to predict the total cost of accidents and classify incidents as high-cost based on a variety of factors such as People, Product, Property, and Environment. 

To implement these findings, the project proposes the development of a Safety & Environment Management System (SEMS) App for Aramco. This app aims to provide employees and managers with tools to transition from a reactive to a proactive approach in managing pipeline risks, thereby improving operational efficiency and mitigating potential environmental impacts.

The report also discusses the limitations of using U.S.-based data and highlights the app’s potential vulnerabilities to human input errors. Future improvements include sourcing Saudi-specific data for model refinement and setting threshold values for continuous variables to improve prediction accuracy.

## Project Objectives

1. Predict whether pipeline accidents will be high-cost.
2. Estimate the total cost of pipeline accidents when they occur.

By achieving these objectives, the project aims to provide Aramco with a comprehensive risk management framework that integrates predictive modeling into its safety protocols.

## Methodology

1. **Data Collection**: Data from PHMSA was sourced due to the unavailability of Saudi-specific data. The dataset includes various factors influencing pipeline incidents, such as environmental impact, product type, and accident location.
2. **Data Cleaning and Preparation**: Data inconsistencies and missing values were handled using placeholders, educated imputation, and, in some cases, row deletion. The dataset was also processed to create new columns and standardize variable formats.
3. **Model Development**:
   - **Cost Prediction Models**: Linear Regression and CART models were developed to predict accident costs.
   - **High-Cost Classification Models**: Logistic Regression and CART models were built to classify high-cost incidents.
4. **Business Implementation**: The SEMS App was proposed as a business implementation tool to leverage predictive insights for proactive safety management.

## Limitations and Future Improvements

While the project demonstrates significant predictive capabilities, it is limited by the use of non-Saudi data, which may not fully reflect the conditions specific to Aramco’s operations. Future improvements include:
- Acquiring and integrating Saudi-specific data for better model accuracy.
- Implementing additional predictive analytics models to further refine cost estimation.
- Regular model retraining to keep up with evolving trends and incidents in the oil industry.

By addressing these limitations, the project can further enhance Aramco's ability to proactively manage pipeline safety and operational risks.

