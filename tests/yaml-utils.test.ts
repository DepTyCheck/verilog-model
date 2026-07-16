import { describe, it, expect } from 'vitest';
import { validateList, validateIssueType } from '../scripts/utils/yaml_utils.js';

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

describe('validateIssueType', () => {
	it('returns null when missing', () => {
		expect(validateIssueType(undefined, 'f.yaml')).toBeNull();
		expect(validateIssueType(null, 'f.yaml')).toBeNull();
	});

	it('returns null for an empty array', () => {
		expect(validateIssueType([], 'f.yaml')).toBeNull();
	});

	it('normalizes a single string to an array', () => {
		expect(validateIssueType('downstream', 'f.yaml')).toEqual(['downstream']);
		expect(validateIssueType('crash', 'f.yaml')).toEqual(['crash']);
	});

	it('accepts an array of valid types', () => {
		expect(validateIssueType(['crash', 'bad_message'], 'f.yaml')).toEqual(['crash', 'bad_message']);
		expect(validateIssueType(['feature', 'downstream'], 'f.yaml')).toEqual(['feature', 'downstream']);
	});

	it('throws on an unknown issue type', () => {
		expect(() => validateIssueType('other', 'f.yaml')).toThrow(/issue_type/);
		expect(() => validateIssueType(['crash', 'other'], 'f.yaml')).toThrow(/f\.yaml/);
	});
});
