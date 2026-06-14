import { describe, it, expect } from 'vitest';
import { validateList } from '../scripts/utils/yaml_utils.js';

describe('validateList', () => {
	it('accepts issues', () => {
		expect(validateList('issues', 'f.yaml')).toBe('issues');
	});

	it('accepts controversial', () => {
		expect(validateList('controversial', 'f.yaml')).toBe('controversial');
	});

	it('throws on a missing list', () => {
		expect(() => validateList(undefined, 'f.yaml')).toThrow(/list/);
	});

	it('throws on an unknown list', () => {
		expect(() => validateList('other', 'f.yaml')).toThrow(/f\.yaml/);
	});
});
