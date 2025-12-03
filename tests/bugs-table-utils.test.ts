import { describe, it, expect } from 'vitest';
import { sortErrors } from '../src/lib/components/bugs-table/bugs-table-utils';
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
            const sorted = sortErrors(mockErrors, mockErrorPercentages, 'title', true);
            expect(sorted.map(e => e.title)).toEqual([
                'Alpha error',
                'Beta error',
                'Gamma error',
                'Zebra error'
            ]);
        });

        it('should sort errors by title in descending order', () => {
            const sorted = sortErrors(mockErrors, mockErrorPercentages, 'title', false);
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
            const sorted = sortErrors(mockErrors, mockErrorPercentages, 'firstFound', true);
            expect(sorted.map(e => e.id)).toEqual([
                'error4', // No date (0)
                'error1', // 2024-01-15
                'error3', // 2024-02-10
                'error2'  // 2024-03-20
            ]);
        });

        it('should sort errors by first found date in descending order', () => {
            const sorted = sortErrors(mockErrors, mockErrorPercentages, 'firstFound', false);
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
            const sorted = sortErrors(mockErrors, mockErrorPercentages, 'stats', true);
            expect(sorted.map(e => e.id)).toEqual([
                'error2', // 0.39%
                'error4', // 0.58%
                'error1', // 29%
                'error3', // 119%
            ]);
        });

        it('should sort errors by overall percentage in descending order', () => {
            const sorted = sortErrors(mockErrors, mockErrorPercentages, 'stats', false);
            expect(sorted.map(e => e.id)).toEqual([
                'error3', // 119%
                'error1', // 29%
                'error4', // 0.58%
                'error2', // 0.39%
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

            const sorted = sortErrors(errorsWithMissingStats, mockErrorPercentages, 'stats', true);
            // error5 has no stats, should be treated as 0 and come first
            expect(sorted[0].id).toBe('error5');
        });
    });

    describe('edge cases', () => {
        it('should handle empty array', () => {
            const sorted = sortErrors([], mockErrorPercentages, 'title', true);
            expect(sorted).toEqual([]);
        });

        it('should handle single element array', () => {
            const singleError = [mockErrors[0]];
            const sorted = sortErrors(singleError, mockErrorPercentages, 'title', true);
            expect(sorted).toEqual(singleError);
        });

        it('should not mutate the original array', () => {
            const original = [...mockErrors];
            sortErrors(mockErrors, mockErrorPercentages, 'title', true);
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

            const sorted = sortErrors(duplicateTitles, {}, 'title', true);
            expect(sorted).toHaveLength(2);
            expect(sorted[0].title).toBe('Same title');
            expect(sorted[1].title).toBe('Same title');
        });
    });
});
