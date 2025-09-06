import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

export function getScriptDir(importMetaUrl) {
  const __filename = fileURLToPath(importMetaUrl);
  const __dirname = path.dirname(__filename);
  return __dirname;
}

export function prepareYamlInputs(importMetaUrl, argv, findYamlFiles, usageMessage) {
  const scriptDir = getScriptDir(importMetaUrl);

  const foundErrorsArg = argv[2];
  if (!foundErrorsArg) {
    console.error(usageMessage || 'Usage: node <script> <path_to_found_errors>');
    process.exit(1);
  }

  const foundErrorsDir = path.resolve(process.cwd(), foundErrorsArg);

  console.log(`Scanning for YAML files in: ${foundErrorsDir}`);

  if (!fs.existsSync(foundErrorsDir)) {
    console.error('found_errors directory does not exist!');
    process.exit(1);
  }

  const yamlFiles = findYamlFiles(foundErrorsDir);
  console.log(`Found ${yamlFiles.length} YAML files`);

  return { scriptDir, foundErrorsDir, yamlFiles };
}


