import { describe, it, expect } from 'vitest';
import { getIssueTypeDisplay } from '../src/lib/index';

describe('getIssueTypeDisplay', () => {
	it('shows Warning in yellow', () => {
		expect(getIssueTypeDisplay('warning')).toEqual({ text: 'Warning', color: 'yellow' });
	});

	it('shows Bad message in orange', () => {
		expect(getIssueTypeDisplay('bad_message')).toEqual({ text: 'Bad message', color: 'orange' });
	});
});
