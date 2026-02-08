import fs from 'fs';
import path from 'path';
import { parse } from 'yaml';

export function findYamlFiles(dir, fileList = []) {
  const files = fs.readdirSync(dir);

  for (const file of files) {
    const filePath = path.join(dir, file);
    const stat = fs.statSync(filePath);

    if (stat.isDirectory()) {
      findYamlFiles(filePath, fileList);
    } else if (file.endsWith('.yaml') || file.endsWith('.yml')) {
      fileList.push(filePath);
    }
  }

  return fileList;
}

function parseDate(dateStr) {
  if (!dateStr) return null;
  const match = /^(\d{2})\.(\d{2})\.(\d{4})$/.exec(dateStr);
  if (match) {
    const [, day, month, year] = match;

    return new Date(`${year}-${month}-${day}`);
  }

  return new Date(dateStr);
}

// Helper function to clean issue_links by removing undefined/null properties
function cleanIssueLinks(issueLinks) {
  if (!Array.isArray(issueLinks)) return [];

  return issueLinks.map(link => {
    const cleanLink = { url: link.url };

    // Only add link_name if it exists and is not null/undefined
    if (link.link_name != null) {
      cleanLink.link_name = String(link.link_name).trim();
    }

    // Only add link_icon if it exists and is not null/undefined
    if (link.link_icon != null) {
      cleanLink.link_icon = link.link_icon;
    }

    return cleanLink;
  });
}

// Function to parse a YAML file and convert it to FoundError format
export function parseYamlFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    const doc = parse(content);

    const { stage, lang, tool, id, short_desc, title, examples, issue_novelty, issue_links, maintainers_response } = doc;

    const parsedExamples = [];
    if (Array.isArray(examples)) {
      for (const example of examples) {
        const entries = Object.entries(example);
        const [exampleId, dataRaw] = entries[0];
        const data = (dataRaw && typeof dataRaw === 'object') ? dataRaw : {};

        parsedExamples.push({
          id: exampleId,
          first_found: data.first_found ? parseDate(data.first_found) : null,
          minified_example: data.minified_example ?? null,
          minified_error: data.minified_error ?? null,
          full_error: data.full_error ?? null,
          full_example: data.full_example ?? null
        });
      }
    }

    return {
      stage: stage,
      lang,
      tool,
      id,
      title,
      short_desc: short_desc || null,
      examples: parsedExamples,
      issue_links: cleanIssueLinks(issue_links),
      issue_novelty: issue_novelty || null,
      maintainers_response: maintainers_response || null,
      issue_type: doc.issue_type || null,
    };
  } catch (error) {
    console.error(`Error parsing ${filePath}:`, error.message);
    process.exit(1);
  }
}


