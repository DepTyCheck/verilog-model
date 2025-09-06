import { prepareYamlInputs } from './utils/cli_utils.js';
import { findYamlFiles, parseYamlFile } from './utils/yaml_utils.js';

function main() {
  const { yamlFiles } = prepareYamlInputs(
    import.meta.url,
    process.argv,
    findYamlFiles,
    'Usage: node scripts/check_unique_ids.js <path_to_found_errors>'
  );

  const seenIds = new Set();

  for (const filePath of yamlFiles) {
    const parsed = parseYamlFile(filePath);
    if (!parsed) continue;

    const { id, examples } = parsed;

    if (id != null) {
      if (seenIds.has(id)) {
        console.error(`Duplicate id detected: ${id} in ${filePath}`);
        process.exit(1);
      }
      seenIds.add(id);
    }

    if (Array.isArray(examples)) {
      for (const ex of examples) {
        const exId = ex && ex.id;
        if (exId == null) continue;
        if (seenIds.has(exId)) {
          console.error(`Duplicate id detected: ${exId} in ${filePath}`);
          process.exit(1);
        }
        seenIds.add(exId);
      }
    }
  }

  console.log('All ids are unique.');
}

main();


