import fs from 'fs';
import { prepareYamlInputs } from './utils/cli_utils.js';
import { findYamlFiles } from './utils/yaml_utils.js';

function main() {
    const { yamlFiles } = prepareYamlInputs(
        import.meta.url,
        process.argv,
        findYamlFiles,
        'Usage: node scripts/add_field_to_all.js <path_to_found_errors>'
    );

    let modifiedCount = 0;

    for (const filePath of yamlFiles) {
        try {
            const content = fs.readFileSync(filePath, 'utf8');
            const lines = content.split('\n');

            // Insert 'lang: sv' as the second line (index 1)
            if (lines.length > 0) {
                lines.splice(1, 0, 'lang: sv');
            }

            const newContent = lines.join('\n');

            // Write back to file
            fs.writeFileSync(filePath, newContent, 'utf8');
            modifiedCount++;
        } catch (err) {
            console.error(`Error processing ${filePath}:`, err);
        }
    }

    console.log(`Successfully added 'lang: sv' to ${modifiedCount} files.`);
}

main();
