import { describe, it, expect } from 'vitest';
import { getToolLink, ErrorStatsCalculator, linkToCommit, getParsedErrorStats } from '../src/lib/components/bugs-table/error-stats-utils';
import testErrorsStatsRaw from './data/test-errors-stats.json';
import { allFoundErrors as prodFoundErrors } from '../src/lib/generated/errors-data';

const testErrorsStats = getParsedErrorStats(testErrorsStatsRaw);


const calculator = new ErrorStatsCalculator(testErrorsStats, prodFoundErrors);

describe('error-stats-utils', () => {
    it('getToolLink returns correct link for iverilog', () => {
        const link = getToolLink('iverilog');
        expect(link).toBe('https://github.com/steveicarus/iverilog');
    });

    it('getToolLink returns undefined for unknown tool', () => {
        const link = getToolLink('unknown-tool');
        expect(link).toBeUndefined();
    });

    it('linkToCommit returns correct link', () => {
        const link = linkToCommit('iverilog', 'abc1234');
        expect(link).toBe('https://github.com/steveicarus/iverilog/tree/abc1234');
    });

    it('linkToCommitForErrStat returns correct link', () => {
        const errStat = testErrorsStats.errors['how_to_elaborate_assignment_pattern_expressions_for_real_type'];
        const link = calculator.linkToCommitForErrStat(errStat);
        expect(link).toBe('https://github.com/steveicarus/iverilog/tree/4f31fec5c8a448d5eace99662da4e49a28b5870e');
    });

    it('findRunsCount returns correct count', () => {
        const from = new Date('2025-11-30T16:30:00');
        const to = new Date('2025-11-30T18:00:00');
        const count = calculator.findRunsCount(from, to);
        expect(count).toBe(256);
    });

    it('runsForErrorsStat returns correct total runs', () => {
        const errStat = testErrorsStats.errors['how_to_elaborate_assignment_pattern_expressions_for_real_type'];
        const runs = calculator.runsForErrorsStat(errStat);
        expect(runs).toBe(512);
    });
});
