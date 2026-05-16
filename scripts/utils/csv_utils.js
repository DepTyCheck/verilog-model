import fs from 'fs';

/**
 * @param {string} content
 * @returns {{ header: string[], rows: string[][] }}
 */
export function parseCsv(content) {
	const lines = content
		.replace(/\r\n?/g, '\n')
		.split('\n')
		.filter((line) => line.length > 0);
	if (lines.length === 0) {
		return { header: [], rows: [] };
	}
	const header = lines[0].split(',');
	const rows = lines.slice(1).map((line) => line.split(','));
	return { header, rows };
}

/**
 * @param {string} filePath
 * @returns {{ header: string[], rows: string[][] }}
 */
export function readCsvFile(filePath) {
	const content = fs.readFileSync(filePath, 'utf8');
	return parseCsv(content);
}
