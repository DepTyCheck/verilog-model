import rawErrorsData from '$lib/data/errors-stats.json';

export type LastErrorOccurrence = {
	commit: string;
	date: Date;
};

export type ErrorStat = {
	error_id: string;
	overall: number;
	test_paths_count: number;
	last: LastErrorOccurrence;
};

export type RunStat = {
	date: Date;
	amount: number;
};

export type ErrorsStats = {
	errors: Record<string, ErrorStat>;
	runs: RunStat[];
};

function getParsedErrorStats(): ErrorsStats {
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	const rawData = rawErrorsData as any;
	return {
		errors: Object.fromEntries(
			// eslint-disable-next-line @typescript-eslint/no-explicit-any
			Object.entries(rawData.errors).map(([errorId, error]: [string, any]) => [
				errorId,
				{
					...error,
					error_id: errorId,
					last: {
						...error.last,
						date: new Date(error.last.date)
					}
				}
			])
		),
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		runs: rawData.runs.map((run: any) => ({
			...run,
			date: new Date(run.date)
		}))
	};
}

export const errorsStats = getParsedErrorStats();
