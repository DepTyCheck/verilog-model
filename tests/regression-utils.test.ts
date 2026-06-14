import { describe, it, expect } from 'vitest';
import { buildReproducedStates, extractLastRunDate } from '../scripts/utils/regression_utils.js';
import { createReproducedChoices } from '../src/lib/components/bugs-table/regression-utils';

// Minimal shape buildReproducedStates needs: { id, examples: [{ id }] }
const foundErrors = [
	{ id: 'err_a', examples: [{ id: 'err_a_ex1' }, { id: 'err_a_ex2' }] },
	{ id: 'err_b', examples: [{ id: 'err_b_ex1' }] },
	{ id: 'err_c', examples: [{ id: 'err_c_ex1' }] }
];

// Rows as parseCsv yields them (header already stripped): [example_id, type, is_reproduced]
const rows = [
	['err_a_ex1', 'full', 'true'],
	['err_a_ex1', 'minified', 'false'],
	['err_a_ex2', 'full', 'false'],
	['err_b_ex1', 'full', 'false'],
	['err_b_ex1', 'minified', 'false']
];

describe('buildReproducedStates', () => {
	const { errorReproducedStates, exampleReproducedStates } = buildReproducedStates(foundErrors, rows);

	it('marks an error reproduced when any example/type reproduced', () => {
		expect(errorReproducedStates['err_a']).toBe('reproduced');
	});

	it('marks an error not_reproduced when rows exist but all are false', () => {
		expect(errorReproducedStates['err_b']).toBe('not_reproduced');
	});

	it('marks an error untested when no rows reference its examples', () => {
		expect(errorReproducedStates['err_c']).toBe('untested');
	});

	it('resolves per-example per-type states', () => {
		expect(exampleReproducedStates['err_a_ex1']).toEqual({
			full: 'reproduced',
			minified: 'not_reproduced'
		});
	});

	it('uses untested for a type with no row', () => {
		expect(exampleReproducedStates['err_a_ex2']).toEqual({
			full: 'not_reproduced',
			minified: 'untested'
		});
	});

	it('marks both types untested for an example with no rows', () => {
		expect(exampleReproducedStates['err_c_ex1']).toEqual({
			full: 'untested',
			minified: 'untested'
		});
	});

	it('ignores csv rows referencing unknown examples', () => {
		const result = buildReproducedStates(foundErrors, [['ghost_ex1', 'full', 'true']]);
		expect(result.exampleReproducedStates['ghost_ex1']).toBeUndefined();
		expect(result.errorReproducedStates['err_a']).toBe('untested');
	});
});

describe('createReproducedChoices', () => {
	it('returns the three reproduction filter choices', () => {
		const choices = createReproducedChoices();
		expect(choices.map((c) => c.value)).toEqual(['reproduced', 'not_reproduced', 'untested']);
		expect(choices.map((c) => c.label)).toEqual(['Reproduced', 'Not reproduced', 'Not tested']);
	});
});

describe('extractLastRunDate', () => {
	it('returns the iso date from valid metadata', () => {
		const raw = '{ "last_regression_test_date": "2026-06-14T06:35:30Z" }';
		expect(extractLastRunDate(raw)).toBe('2026-06-14T06:35:30Z');
	});

	it('returns null for null input', () => {
		expect(extractLastRunDate(null)).toBeNull();
	});

	it('returns null when the field is absent', () => {
		expect(extractLastRunDate('{ "other": 1 }')).toBeNull();
	});

	it('returns null for unparseable json', () => {
		expect(extractLastRunDate('not json')).toBeNull();
	});
});
