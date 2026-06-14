/**
 * @typedef {'reproduced' | 'not_reproduced' | 'untested'} ReproducedState
 */

/**
 * Pure resolution of regression-test results to per-error and per-example states.
 *
 * @param {{ id: string, examples: { id: string }[] }[]} foundErrors
 * @param {string[][]} csvRows  rows of [example_id, type, is_reproduced], header already stripped
 * @returns {{
 *   errorReproducedStates: Record<string, ReproducedState>,
 *   exampleReproducedStates: Record<string, { full: ReproducedState, minified: ReproducedState }>
 * }}
 */
export function buildReproducedStates(foundErrors, csvRows) {
	// csvMap[example_id][type] = boolean
	/** @type {Record<string, { full?: boolean, minified?: boolean }>} */
	const csvMap = {};
	for (const row of csvRows) {
		const exampleId = (row[0] ?? '').trim();
		const type = (row[1] ?? '').trim();
		const isReproduced = (row[2] ?? '').trim() === 'true';
		if (!exampleId || (type !== 'full' && type !== 'minified')) continue;
		(csvMap[exampleId] ??= {})[type] = isReproduced;
	}

	/**
	 * @param {boolean | undefined} value
	 * @returns {ReproducedState}
	 */
	const stateOf = (value) => {
		if (value === undefined) return 'untested';
		return value ? 'reproduced' : 'not_reproduced';
	};

	/** @type {Record<string, ReproducedState>} */
	const errorReproducedStates = {};
	/** @type {Record<string, { full: ReproducedState, minified: ReproducedState }>} */
	const exampleReproducedStates = {};

	for (const error of foundErrors) {
		let anyReproduced = false;
		let anyTested = false;

		for (const example of error.examples ?? []) {
			const entry = csvMap[example.id] ?? {};
			const fullState = stateOf(entry.full);
			const minifiedState = stateOf(entry.minified);
			exampleReproducedStates[example.id] = { full: fullState, minified: minifiedState };

			for (const s of [fullState, minifiedState]) {
				if (s !== 'untested') anyTested = true;
				if (s === 'reproduced') anyReproduced = true;
			}
		}

		errorReproducedStates[error.id] = anyReproduced ? 'reproduced' : anyTested ? 'not_reproduced' : 'untested';
	}

	return { errorReproducedStates, exampleReproducedStates };
}

/**
 * Pull the last regression-run date out of latest_run_metadata.json content.
 * Degrades to null on any problem — the file is produced by an external job.
 *
 * @param {string | null} raw  file contents, or null when the file is absent
 * @returns {string | null}
 */
export function extractLastRunDate(raw) {
	if (raw == null) return null;
	try {
		const meta = JSON.parse(raw);
		const date = meta?.last_regression_test_date;
		return typeof date === 'string' && date.length > 0 ? date : null;
	} catch {
		return null;
	}
}
