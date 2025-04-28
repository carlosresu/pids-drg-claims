# Define the notebook to run
NOTEBOOK_TO_RUN = "02-drg-cleaning-v3"

import subprocess
import nbformat
from nbconvert import ScriptExporter
from pathlib import Path
from concurrent.futures import ProcessPoolExecutor  # Enables parallel execution

# Define paths
base_dir = Path.home() / "pids-drg-claims" / "data-cleaning"
debug_dir = base_dir / "debug"
cache_dir = debug_dir / "cache"

# Ensure cache directory exists
cache_dir.mkdir(parents=True, exist_ok=True)

# Function to convert the Jupyter notebook to an R script
def convert_notebook_to_r(notebook_name):
    notebook_path = base_dir / f"{notebook_name}.ipynb"
    converted_r_script = debug_dir / f"{notebook_name}.r"

    if notebook_path.exists():
        with open(notebook_path, "r", encoding="utf-8") as nb_file:
            nb_content = nbformat.read(nb_file, as_version=4)

        # Convert only if the notebook uses R
        if nb_content.get("metadata", {}).get("kernelspec", {}).get("language", "") == "R":
            script_content, _ = ScriptExporter().from_notebook_node(nb_content)

            # Save converted script
            with open(converted_r_script, "w", encoding="utf-8") as r_file:
                r_file.write(script_content)

            print(f"Converted {notebook_path.name} to {converted_r_script}")
        else:
            print("Error: Notebook does not use R.")
    else:
        print("Error: Notebook file not found.")

    return converted_r_script  # Return script path for execution

# Function to run an R script for a given year_to_load
def run_notebook(year_to_load):
    converted_r_script = convert_notebook_to_r(NOTEBOOK_TO_RUN)
    year_file = cache_dir / "year_to_load.txt"

    # Write the current year_to_load to a cache file
    with open(year_file, "w", encoding="utf-8") as f:
        f.write(str(year_to_load))

    automate_file = cache_dir / "automate.txt"

    # Create the automate flag
    with open(automate_file, "w", encoding="utf-8") as f:
        f.write("TRUE")

    print(f"Running R script for year_to_load: {year_to_load}")

    result = subprocess.run(["Rscript", str(converted_r_script)], capture_output=True, text=True)

    # Remove automate flag after execution
    if automate_file.exists():
        automate_file.unlink()

    if result.returncode != 0:
        print(f"Error running {converted_r_script} for {year_to_load}. Exit code: {result.returncode}")
        return False
    return True

# Run a years; with options for parallel execution
years = list(range(2024, 2025))  # List of years to process
with ProcessPoolExecutor(max_workers=1) as executor:
    results = list(executor.map(run_notebook, years))  # Execute in parallel

# Check if any run failed
if not all(results):
    raise RuntimeError("One or more R scripts failed.")
