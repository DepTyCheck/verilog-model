<script lang="ts">
	import { A, Tooltip } from 'flowbite-svelte';
	import type { ErrorsStats } from '$lib/errors-utils';
	import { formatDateDMY } from '$lib/index';

	export let errorId: string;
	export let errorsStats: ErrorsStats;

	const errorStat = errorsStats.errors[errorId];
	const totalRuns = errorsStats.runs.reduce((sum: number, run) => sum + run.amount, 0);
	const percentage = errorStat && totalRuns > 0 ? ((errorStat.count / totalRuns) * 100).toFixed(1) : '0';
</script>


<div class="text-sm space-y-2">
    <div class="border-b border-gray-200 pb-2">
            {percentage}% occurrence
    </div>
    
    <div class="space-y-1">
        <div class="text-gray-600">
            Last: {formatDateDMY(errorStat.last.date)}
        </div>
        <div class="text-sm">
            <A 
                href={errorStat.last.link} 
                rel="noopener noreferrer"
            >
                Commit {errorStat.last.commit.substring(0, 7)}
            </A>
        </div>
    </div>
</div>
<Tooltip type="auto">{`This issue was found ${errorStat.count} times out of ${totalRuns} total test runs since DD.MM.YYYY`}</Tooltip>

