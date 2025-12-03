import toolLinks from '$lib/data/tools-git-links.json';
import { errorsStats as prodErrorsStats } from '$lib/parsed-error-stats';
import type { ErrorStat, ErrorsStats } from '$lib/parsed-error-stats';
import { allFoundErrors as prodFoundErrors } from '$lib/generated/errors-data';
import { getFirstFound } from '$lib/index';
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

export class ErrorStatsCalculator {
	constructor(
		private errorsStats: ErrorsStats,
		private foundErrors: FoundError[]
	) {}

	linkToCommitForErrStat(errStat: ErrorStat): string {
		const foundError = this.foundErrors.find((e) => e.id === errStat.error_id);
		return linkToCommit(foundError!.tool, errStat.last.commit);
	}

	findRunsCount(from: Date, to: Date): number {
		const relevantRuns = this.errorsStats.runs.filter((run) => run.date >= from && run.date <= to);
		return relevantRuns.reduce((sum, run) => sum + run.amount, 0);
	}

	runsForErrorsStat(errStat: ErrorStat): number {
		const foundError = this.foundErrors.find((e) => e.id == errStat.error_id);
		const firstFound = getFirstFound(foundError!);
		const totalRuns = this.findRunsCount(firstFound!, errStat.last.date);
		return totalRuns;
	}

	issueOccurancePct(errStat: ErrorStat): ErrorPercentages {
		const totalRuns = this.runsForErrorsStat(errStat);
		let pcts = [errStat.overall, errStat.test_paths_count].map((count) =>
			parseFloat(((count / totalRuns) * 100).toFixed(2))
		);
		return { overall: pcts[0], testFiles: pcts[1], totalRuns };
	}

	calculateErrorPercentages(): Record<string, ErrorPercentages> {
		const result: Record<string, ErrorPercentages> = {};
		for (const [errorId, errorStat] of Object.entries(this.errorsStats.errors)) {
			try {
				const { overall, testFiles, totalRuns } = this.issueOccurancePct(errorStat);
				result[errorId] = { overall, testFiles, totalRuns };
			} catch (e) {}
		}
		return result;
	}
}

export const defaultCalculator = new ErrorStatsCalculator(prodErrorsStats, prodFoundErrors);

export function linkToCommitForErrStat(errStat: ErrorStat): string {
	return defaultCalculator.linkToCommitForErrStat(errStat);
}

export function findRunsCount(from: Date, to: Date): number {
	return defaultCalculator.findRunsCount(from, to);
}

export function runsForErrorsStat(errStat: ErrorStat): number {
	return defaultCalculator.runsForErrorsStat(errStat);
}

export function issueOccurancePct(errStat: ErrorStat): ErrorPercentages {
	return defaultCalculator.issueOccurancePct(errStat);
}

export const errorPercentages = defaultCalculator.calculateErrorPercentages();
