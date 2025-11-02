reticulate::install_miniconda()
python_path <- readLines("python_path.txt")
reticulate::use_python(python_path)
reticulate::conda_create("textrpp_reticulate", packages = "python=3.9")
rpp_packages <- c(
  "torch==2.2.0",
  "transformers==4.38.0",
  "huggingface_hub==0.20.0",
  "numpy==1.26.0",
  "pandas==2.0.3",
  "nltk==3.8.1",
  "scikit-learn==1.3.0",
  "datasets==2.16.1",
  "evaluate==0.4.0",
  "accelerate==0.26.0",
  "bertopic==0.16.3",
  "jsonschema==4.19.2",
  "sentence-transformers==2.2.2",
  "flair==0.13.0",
  "umap-learn==0.5.6",
  "hdbscan==0.8.33",
  "scipy==1.10.1",
  "aiohappyeyeballs==2.4.4"
)

# Use Conda Forge for Package hdbscan
reticulate::conda_install(
  envname = "textrpp_reticulate",
  packages = "hdbscan",
  channel = "conda-forge"
)

# Install the Rest of the Packages normally
reticulate::conda_install(
  "textrpp_reticulate",
  packages = rpp_packages[!grepl("hdbscan", rpp_packages)],
  pip = TRUE
)

# Show available Conda environments
reticulate::conda_list()

# Initialize the installed Conda environment
text::textrpp_initialize(condaenv = "textrpp_reticulate", save_profile = TRUE)

# Test that textEmbed works
test_embedding <- textEmbed(
  "hello",
  model = "manifesto-project/manifestoberta-xlm-roberta-56policy-topics-context-2024-1-1"
) # this fails

# Try another Model
test_embedding <- textEmbed(
  "hello",
  model = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
) # this works


# Check GPUs -------------------------------------------------------------

library(torch)
install_torch()
cuda_is_available()
