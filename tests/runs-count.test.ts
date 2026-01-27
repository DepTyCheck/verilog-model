import { describe, it, expect } from 'vitest';
import { getParsedErrorStats } from '../src/lib/parsed-error-stats';
import { ErrorStatsCalculator } from '../src/lib/components/bugs-table/error-stats-utils';
import testErrorsStatsRaw from './data/test-runs-count/errors-stats-state-2026-01-26.json';
import { allFoundErrors as prodFoundErrors } from '../src/lib/generated/errors-data';

const testErrorsStats = getParsedErrorStats(testErrorsStatsRaw);


const calculator = new ErrorStatsCalculator(testErrorsStats, prodFoundErrors);

describe('runs-count', () => {
    it('errors found before runs started should have equal number of runs', () => {
        const conflictingInitError = testErrorsStats.errors['conflicting_init_values_for_signal'];
        const netTypeError = testErrorsStats.errors['net_type_not_supported'];

        const conflictingInitRuns = calculator.runsForErrorsStat(conflictingInitError);
        const netTypeRuns = calculator.runsForErrorsStat(netTypeError);

        expect(conflictingInitRuns).toBe(netTypeRuns);
        expect(netTypeRuns).toBe(14336);
    });
});
