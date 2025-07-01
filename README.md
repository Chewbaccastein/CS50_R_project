# 📊 chartsmith

**chartsmith** is a lightweight and user-friendly R package that helps you generate quick, clean, and customizable charts from CSV files. With just a few inputs, you can visualize your data using bar charts, line graphs, or donut plots—no complex setup or coding required.

![chartsmith demo](https://user-images.githubusercontent.com/your_demo_gif_here.gif) <!-- Optional: link to a demo gif or screenshot -->

---

## ✨ Features

- 📂 Import CSV data easily
- 📉 Create bar, line, and donut charts with minimal code
- 🧠 Beginner-friendly interface and syntax
- 🧪 Built-in tests with `testthat`
- 🖥️ Optional interactive CLI (`start_chartsmith()`) for hands-on chart building

---

## 📦 Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("your-username/chartsmith")
```

### Limitations in Version 1.0.0
Version 1.0.0 is designed to be minimal and accessible, but comes with the following limitations:

Only three chart types supported (bar, line, donut)

No built-in data cleaning or formatting tools

Assumes CSV files are relatively clean and well-structured

Limited styling or customization options

### Roadmap / Planned Features
Planned updates for future versions include:

📈 Additional chart types (scatter plots, histograms, stacked bars, etc.)

🧹 Optional data-cleaning steps (e.g., trimming whitespace, removing NA)

🎨 More chart customization (themes, colors, labels, interactivity)

🧩 Faceting and group-based visualizations

📦 CRAN submission support

### 📜 License
MIT © Hansol Park

