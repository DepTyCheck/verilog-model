import fs from 'fs';
import { prepareYamlInputs } from './utils/cli_utils.js';
import { findYamlFiles } from './utils/yaml_utils.js';

function extractIssueNumber(url) {
    // Extract issue number from GitHub URL
    // Examples:
    // https://github.com/povik/yosys-slang/issues/181#issuecomment-3161067098 -> #181
    // https://github.com/povik/yosys-slang/issues/123 -> #123
    const match = url.match(/\/issues\/(\d+)/);
    return match ? `#${match[1]}` : null;
}

function main() {
    const { yamlFiles } = prepareYamlInputs(
        import.meta.url,
        process.argv,
        findYamlFiles,
        'Usage: node scripts/migration_issue_link_to_links.js <path_to_found_errors>'
    );

    let modifiedCount = 0;
    let skippedCount = 0;

    for (const filePath of yamlFiles) {
        try {
            const content = fs.readFileSync(filePath, 'utf8');
            const lines = content.split('\n');
            let modified = false;

            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];

                // Look for lines that match "issue_link: <url>"
                const match = line.match(/^issue_link:\s+(.+)$/);

                if (match) {
                    const url = match[1].trim();
                    const issueNumber = extractIssueNumber(url);

                    if (issueNumber) {
                        // Replace the single line with the new multi-line format
                        // Create a single list item with url and title properties
                        const replacement = [
                            'issue_links:',
                            `  - url: ${url}`,
                            `    link_name: >`,
                            `      ${issueNumber}`
                        ];

                        // Replace the current line with the new format
                        lines.splice(i, 1, ...replacement);

                        modified = true;
                        console.log(`Migrated ${filePath}: ${issueNumber}`);
                    } else {
                        console.warn(`Could not extract issue number from ${url} in ${filePath}`);
                    }

                    // Don't need to check further lines in this file
                    break;
                }
            }

            if (modified) {
                const newContent = lines.join('\n');
                fs.writeFileSync(filePath, newContent, 'utf8');
                modifiedCount++;
            } else {
                skippedCount++;
            }
        } catch (err) {
            console.error(`Error processing ${filePath}:`, err);
        }
    }

    console.log(`\nMigration complete!`);
    console.log(`Successfully migrated: ${modifiedCount} files`);
    console.log(`Skipped (no issue_link found): ${skippedCount} files`);
}

main();
