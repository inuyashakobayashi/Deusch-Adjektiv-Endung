import json
from collections import defaultdict

def convert_to_json(input_file, output_file):
    words_dict = {}
    total_words = set()
    categories = {
        "missing_all": set(),
        "missing_gender": set(),
        "missing_genitive": set(),
        "missing_plural": set(),
        "complete": set()
    }

    # Read and process the input file
    with open(input_file, 'r', encoding='utf-8') as f:
        for line in f:
            word, form, grammar = line.strip().split('\t')
            total_words.add(word)
            
            if word not in words_dict:
                words_dict[word] = {
                    "word": word,
                    "gender": "",
                    "plural": None,
                    "genitive": ""
                }

            # Process grammar info
            if any(gender in grammar for gender in ["MASC", "FEM", "NEUT"]):
                if "MASC" in grammar:
                    words_dict[word]["gender"] = "m"
                elif "FEM" in grammar:
                    words_dict[word]["gender"] = "f"
                elif "NEUT" in grammar:
                    words_dict[word]["gender"] = "n"

            if "PL" in grammar and "NOM" in grammar:
                words_dict[word]["plural"] = form

            if "GEN" in grammar and "SG" in grammar:
                words_dict[word]["genitive"] = f"des {form}" if words_dict[word]["gender"] in ["m", "n"] else f"der {form}"

    # Filter and prepare final list
    complete_nouns = []
    for word, data in words_dict.items():
        if data["gender"] and data["genitive"]:  # Only include words with gender and genitive
            complete_nouns.append(data)

    # Write to JSON file - Changed to write direct array instead of object
    with open(output_file + '.json', 'w', encoding='utf-8') as f:
        json.dump(complete_nouns, f, ensure_ascii=False, indent=2)  # Removed the {"nouns": ...} wrapper

    # Print statistics
    print(f"\nDetailed Conversion Statistics:")
    print(f"Total unique words processed: {len(total_words)}")
    print(f"Successfully converted: {len(complete_nouns)}")

# Use the converter
if __name__ == "__main__":
    convert_to_json('deu', 'german_nouns')