import json
import sys

def clear_notebook_metadata(input_path, output_path):
    # Load the notebook
    with open(input_path, 'r', encoding='utf-8') as f:
        notebook = json.load(f)
    
    # Iterate through the cells and clear all metadata
    for cell in notebook.get('cells', []):
        if 'metadata' in cell:
            cell['metadata'] = {}  # Clear metadata for code and markdown cells
        
        # Optionally, clear execution_count for code cells to reset execution history
        if cell['cell_type'] == 'code':
            cell['execution_count'] = None
            if 'outputs' in cell:
                cell['outputs'] = []  # Clear any outputs from the code cells

    # Save the cleaned notebook
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(notebook, f, indent=2, ensure_ascii=False)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python clear_metadata.py <input_notebook> <output_notebook>")
        sys.exit(1)
    
    input_notebook = sys.argv[1]
    output_notebook = sys.argv[2]

    clear_notebook_metadata(input_notebook, output_notebook)
    print(f"Cleaned notebook saved to {output_notebook}")