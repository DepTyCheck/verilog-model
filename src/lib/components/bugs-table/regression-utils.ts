import type { CheckBoxChoice } from '$lib/core';
import type { ReproducedState } from '$lib/generated/errors-regression';

export const reproducedDisplay: Record<ReproducedState, { label: string; tooltip: string }> = {
	reproduced: {
		label: 'Reproduced',
		tooltip: 'Still reproduces in the latest regression run'
	},
	not_reproduced: {
		label: 'Not reproduced',
		tooltip: 'No longer reproduces in the latest regression run'
	},
	untested: {
		label: 'Not tested',
		tooltip: 'Not tested in the latest regression run'
	}
};

export function createReproducedChoices(): CheckBoxChoice[] {
	return [
		{ value: 'reproduced', label: reproducedDisplay.reproduced.label },
		{ value: 'not_reproduced', label: reproducedDisplay.not_reproduced.label },
		{ value: 'untested', label: reproducedDisplay.untested.label }
	];
}
