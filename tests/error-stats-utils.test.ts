import { describe, it, expect } from 'vitest';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import {
	getToolLink,
	linkToCommit,
	linkToCommitForErrStatWith,
	calculateErrorPercentages
} from '../src/lib/components/bugs-table/error-stats-utils';
import type { ErrorStat } from '../src/lib/generated/errors-stats';
import { allFoundErrors as prodFoundErrors } from '../src/lib/generated/errors-data';
import { parseCsv } from '../scripts/utils/csv_utils.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixturePath = path.join(__dirname, 'data', 'test-combined-stats.csv');
const csvContent = fs.readFileSync(fixturePath, 'utf8');
const { rows } = parseCsv(csvContent);

const fixtureStats: Record<string, ErrorStat> = {};
for (const row of rows) {
	const [error_id, runs, overall, test_files_count, last_commit, last_date] = row;
	fixtureStats[error_id] = {
		error_id,
		overall: Number(overall),
		test_files_count: Number(test_files_count),
		runs: Number(runs),
		last_commit,
		last_date
	};
}

describe('error-stats-utils', () => {
	it('getToolLink returns correct link for iverilog', () => {
		expect(getToolLink('iverilog')).toBe('https://github.com/steveicarus/iverilog');
	});

	it('getToolLink returns undefined for unknown tool', () => {
		expect(getToolLink('unknown-tool')).toBeUndefined();
	});

	it('linkToCommit returns correct link', () => {
		expect(linkToCommit('iverilog', 'abc1234')).toBe(
			'https://github.com/steveicarus/iverilog/tree/abc1234'
		);
	});

	it('linkToCommitForErrStatWith resolves target via foundErrors', () => {
		const errStat = fixtureStats['how_to_elaborate_assignment_pattern_expressions_for_real_type'];
		const link = linkToCommitForErrStatWith(errStat, prodFoundErrors);
		expect(link).toBe(
			'https://github.com/steveicarus/iverilog/tree/4f31fec5c8a448d5eace99662da4e49a28b5870e'
		);
	});

	it('calculateErrorPercentages produces overall/testFiles/totalRuns', () => {
		const pct = calculateErrorPercentages(fixtureStats);
		const row = pct['how_to_elaborate_assignment_pattern_expressions_for_real_type'];
		expect(row.totalRuns).toBe(1000);
		expect(row.overall).toBeCloseTo(25, 5); // 250 / 1000 * 100
		expect(row.testFiles).toBeCloseTo(8, 5); // 80 / 1000 * 100
	});

	it('calculateErrorPercentages skips entries with zero runs', () => {
		const zeroStat: ErrorStat = {
			error_id: 'fake',
			overall: 5,
			test_files_count: 2,
			runs: 0,
			last_commit: 'deadbeef',
			last_date: '2026-01-01'
		};
		const pct = calculateErrorPercentages({ fake: zeroStat });
		expect(pct['fake']).toBeUndefined();
	});
});
