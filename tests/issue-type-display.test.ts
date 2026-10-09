import { describe, it, expect } from 'vitest';
import { getIssueTypeDisplay, getIssueTypeTooltip } from '../src/lib/index';
import type { IssueType } from '../src/lib/core';

describe('getIssueTypeTooltip', () => {
	it.each<[IssueType, string]>([
		['bad_message', 'The message incorrectly describes the problem'],
		['crash', 'The tool failed unexpectedly'],
		['feature', 'The tool behaviour deviates from the specification without documentation'],
		['downstream', 'An issue triggered by an upstream failure'],
		['warning', 'Warning returned by the tool'],
		['infinite_loop', 'Infinite processing loop'],
		[null, '']
	])('describes %s', (type, description) => {
		expect(getIssueTypeTooltip(type)).toBe(description);
	});
});

describe('getIssueTypeDisplay', () => {
	it('shows Warning in yellow', () => {
		expect(getIssueTypeDisplay('warning')).toEqual({ text: 'Warning', color: 'yellow' });
	});

	it('shows Bad message in orange', () => {
		expect(getIssueTypeDisplay('bad_message')).toEqual({ text: 'Bad message', color: 'orange' });
	});
});
