import toolLinks from '$lib/data/tools-git-links.json';
import { errorsStats as prodErrorsStats } from '$lib/generated/errors-stats';
import type { ErrorStat } from '$lib/generated/errors-stats';
import { allFoundErrors as prodFoundErrors } from '$lib/generated/errors-data';
import type { FoundError } from '$lib/core';

export function getToolLink(toolName: string): string {
	return (toolLinks as Record<string, string>)[toolName];
}

export function linkToCommit(toolName: string, hash: string): string {
	const baseLink = getToolLink(toolName);
	return `${baseLink}/tree/${hash}`;
}

export type ErrorPercentages = {
	overall: number;
	testFiles: number;
	totalRuns: number;
};

export function linkToCommitForErrStatWith(
	errStat: ErrorStat,
	foundErrors: FoundError[]
): string {
	const foundError = foundErrors.find((e) => e.id === errStat.error_id);
	return linkToCommit(foundError!.target, errStat.last_commit);
}

export function calculateErrorPercentages(
	errorsStats: Record<string, ErrorStat>
): Record<string, ErrorPercentages> {
	const result: Record<string, ErrorPercentages> = {};
	for (const [errorId, errStat] of Object.entries(errorsStats)) {
		if (errStat.runs === 0) continue;
		result[errorId] = {
			overall: (errStat.overall / errStat.runs) * 100,
			testFiles: (errStat.test_files_count / errStat.runs) * 100,
			totalRuns: errStat.runs
		};
	}
	return result;
}

export function linkToCommitForErrStat(errStat: ErrorStat): string {
	return linkToCommitForErrStatWith(errStat, prodFoundErrors);
}

export const errorPercentages = calculateErrorPercentages(prodErrorsStats);
