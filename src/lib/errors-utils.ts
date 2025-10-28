import rawErrorsData from '$lib/data/errors-data.json';

export type LastErrorOccurrence = {
    commit: string;
    link: string;
    date: Date;
}

export type ErrorStat = {
    error_id: string;
    count: number;
    last: LastErrorOccurrence;
}

export type RunStat = {
    date: Date;
    amount: number;
}

export type ErrorsStats = {
    errors: Record<string, ErrorStat>;
    runs: RunStat[];
}

export function getProcessedErrorsData(): ErrorsStats {
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	const rawData = rawErrorsData as any;
	return {
		errors: Object.fromEntries(
			// eslint-disable-next-line @typescript-eslint/no-explicit-any
			Object.entries(rawData.errors).map(([errorId, error]: [string, any]) => [
				errorId,
				{
					...error,
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
