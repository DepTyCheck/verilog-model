import { describe, it, expect } from 'vitest';
import { SortedIssues } from '../src/lib/components/bugs-table/bugs-table-utils';
import type { FoundError } from '../src/lib/core';
import type { ErrorPercentages } from '../src/lib/components/bugs-table/error-stats-utils';

// Mock data for testing
const mockErrors: FoundError[] = [
    {
        id: 'error1',
        title: 'Zebra error',
        tool: 'iverilog',
        issue_novelty: 'new',
        maintainers_response: 'bug',
        examples: [
            {
                id: 'ex1',
                first_found: '2024-01-15T10:00:00Z'
            }
        ]
    },
    {
        id: 'error2',
        title: 'Alpha error',
        tool: 'verilator',
        issue_novelty: 'already_known',
        maintainers_response: 'enhancement',
        examples: [
            {
                id: 'ex2',
                first_found: '2024-03-20T10:00:00Z'
            }
        ]
    },
    {
        id: 'error3',
        title: 'Beta error',
        tool: 'iverilog',
        issue_novelty: 'new',
        maintainers_response: null,
        examples: [
            {
                id: 'ex3',
                first_found: '2024-02-10T10:00:00Z'
            }
        ]
    },
    {
        id: 'error4',
        title: 'Gamma error',
        tool: 'verilator',
        issue_novelty: null,
        maintainers_response: 'low',
        examples: [
            {
                id: 'ex4'
                // No first_found date
            }
        ]
    }
];

const mockErrorPercentages: Record<string, ErrorPercentages> = {
    error1: {
        overall: 0.78,
        testFiles: 999,
        totalRuns: 256
    },
    error2: {
        overall: 0.39,
        testFiles: 999,
        totalRuns: 256
    },
    error3: {
        overall: 1.56,
        testFiles: 999,
        totalRuns: 256
    },
    error4: {
        overall: 0.2,
        testFiles: 999,
        totalRuns: 512
    }
};

describe('sortErrors', () => {
    describe('sorting by title', () => {
        it('should sort errors by title in ascending order', () => {
            const sorted = new SortedIssues(mockErrors, mockErrorPercentages).sorted('title', 'asc');
            expect(sorted.map(e => e.title)).toEqual([
                'Alpha error',
                'Beta error',
                'Gamma error',
                'Zebra error'
            ]);
        });

        it('should sort errors by title in descending order', () => {
            const sorted = new SortedIssues(mockErrors, mockErrorPercentages).sorted('title', 'desc');
            expect(sorted.map(e => e.title)).toEqual([
                'Zebra error',
                'Gamma error',
                'Beta error',
                'Alpha error'
            ]);
        });
    });

    describe('sorting by firstFound', () => {
        it('should sort errors by first found date in ascending order', () => {
            const sorted = new SortedIssues(mockErrors, mockErrorPercentages).sorted('firstFound', 'asc');
            expect(sorted.map(e => e.id)).toEqual([
                'error4', // No date (0)
                'error1', // 2024-01-15
                'error3', // 2024-02-10
                'error2'  // 2024-03-20
            ]);
        });

        it('should sort errors by first found date in descending order', () => {
            const sorted = new SortedIssues(mockErrors, mockErrorPercentages).sorted('firstFound', 'desc');
            expect(sorted.map(e => e.id)).toEqual([
                'error2', // 2024-03-20
                'error3', // 2024-02-10
                'error1', // 2024-01-15
                'error4'  // No date (0)
            ]);
        });
    });

    describe('sorting by stats', () => {
        it('should sort errors by overall percentage in ascending order', () => {
            const sorted = new SortedIssues(mockErrors, mockErrorPercentages).sorted('stats', 'asc');
            expect(sorted.map(e => e.id)).toEqual([
                'error4', // 0.2%
                'error2', // 0.39%
                'error1', // 0.78%
                'error3', // 1.56%
            ]);
        });

        it('should sort errors by overall percentage in descending order', () => {
            const sorted = new SortedIssues(mockErrors, mockErrorPercentages).sorted('stats', 'desc');
            expect(sorted.map(e => e.id)).toEqual([
                'error3', // 1.56%
                'error1', // 0.78%
                'error2', // 0.39%
                'error4', // 0.2%
            ]);
        });

        it('should handle errors with no stats data', () => {
            const errorsWithMissingStats: FoundError[] = [
                ...mockErrors,
                {
                    id: 'error5',
                    title: 'Error without stats',
                    tool: 'iverilog',
                    issue_novelty: null,
                    maintainers_response: null,
                    examples: []
                }
            ];

            const sorted = new SortedIssues(errorsWithMissingStats, mockErrorPercentages).sorted('stats', 'asc');
            // error5 has no stats, should be treated as 0 and come first
            expect(sorted[0].id).toBe('error5');
        });
    });

    describe('edge cases', () => {
        it('should handle empty array', () => {
            const sorted = new SortedIssues([], mockErrorPercentages).sorted('title', 'asc');
            expect(sorted).toEqual([]);
        });

        it('should handle single element array', () => {
            const singleError = [mockErrors[0]];
            const sorted = new SortedIssues(singleError, mockErrorPercentages).sorted('title', 'asc');
            expect(sorted).toEqual(singleError);
        });

        it('should not mutate the original array', () => {
            const original = [...mockErrors];
            new SortedIssues(mockErrors, mockErrorPercentages).sorted('title', 'asc');
            expect(mockErrors).toEqual(original);
        });

        it('should handle errors with identical sort values', () => {
            const duplicateTitles: FoundError[] = [
                {
                    id: 'dup1',
                    title: 'Same title',
                    tool: 'iverilog',
                    issue_novelty: null,
                    maintainers_response: null,
                    examples: []
                },
                {
                    id: 'dup2',
                    title: 'Same title',
                    tool: 'verilator',
                    issue_novelty: null,
                    maintainers_response: null,
                    examples: []
                }
            ];

            const sorted = new SortedIssues(duplicateTitles, {}).sorted('title', 'asc');
            expect(sorted).toHaveLength(2);
            expect(sorted[0].title).toBe('Same title');
            expect(sorted[1].title).toBe('Same title');
        });
    });
});
