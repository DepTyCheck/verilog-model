import fs from 'fs';
import path from 'path';
import { findYamlFiles, parseYamlFile } from './utils/yaml_utils.js';
import { prepareYamlInputs } from './utils/cli_utils.js';

function extractSystemVerilogExamples(errors) {
  const svExamples = [];
  
  for (const error of errors) {
      for (const example of error.examples) {
        if (example.full_example && example.id) {
          svExamples.push({
            id: example.id,
            content: example.full_example
          });
        }
      }
  }
  
  return svExamples;
}

function createSystemVerilogFiles(svExamples, outputDir) {
  fs.mkdirSync(outputDir, { recursive: true });
  
  for (const example of svExamples) {
    const fileName = `${example.id}.sv`;
    const filePath = path.join(outputDir, fileName);
    
    fs.writeFileSync(filePath, example.content);
    console.log(`Created: ${fileName}`);
  }
}

function main() {
  const { yamlFiles } = prepareYamlInputs(
    import.meta.url,
    process.argv,
    findYamlFiles,
    'Usage: node scripts/extract_sv_examples.js <path_to_found_errors> <output_directory>'
  );
  
  const outputDirArg = process.argv[3];
  if (!outputDirArg) {
    console.error('Usage: node scripts/extract_sv_examples.js <path_to_found_errors> <output_directory>');
    process.exit(1);
  }
    
  const errors = yamlFiles.map(filePath => parseYamlFile(filePath));
  console.log(`Successfully parsed ${errors.length} error files`);
  
  const svExamples = extractSystemVerilogExamples(errors);
  console.log(`Found ${svExamples.length} SystemVerilog examples`);

  const outputDir = path.resolve(process.cwd(), outputDirArg);
  console.log(`Extracting SystemVerilog examples to: ${outputDir}`);
  
  createSystemVerilogFiles(svExamples, outputDir);
  console.log('Script completed successfully!');
}

main();
