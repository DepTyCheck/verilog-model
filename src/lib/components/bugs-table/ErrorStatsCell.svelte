<script lang="ts">
	import { A, Tooltip } from 'flowbite-svelte';
	import type { ErrorsStats } from '$lib/parsed-error-stats';
	import type { ErrorPercentages } from '$lib/components/bugs-table/error-stats-utils';
	import { formatDateDMY } from '$lib/index';

	import { linkToCommitForErrStat } from '$lib/components/bugs-table/error-stats-utils';

	export let errorId: string;
	export let errorsStats: ErrorsStats;
	export let percentages: ErrorPercentages | undefined = undefined;

	const errorStat = errorsStats.errors[errorId];

	let overallPercentage = 0;
	let testPathsPercentage = 0;
	let totalRuns = 0;
	let linkToolGithub = '';
	let errorsPerFailedTest = '0';

	try {
		totalRuns = percentages!.totalRuns;
		overallPercentage = percentages!.overall;
		testPathsPercentage = percentages!.testFiles;
		errorsPerFailedTest = (errorStat.overall / errorStat.test_paths_count).toFixed(2);

		linkToolGithub = linkToCommitForErrStat(errorStat);
	} catch (e) {}

	const lastCommit = errorStat && errorStat.last ? errorStat.last.commit.substring(0, 7) : '';
	const lastDate = errorStat && errorStat.last ? formatDateDMY(errorStat.last.date) : '';
</script>

{#if errorStat}
	<div class="space-y-2 text-sm">
		<div class="whitespace-nowrap">
			{overallPercentage}% / {testPathsPercentage}% / {errorsPerFailedTest}
		</div>

		<div class="whitespace-nowrap">
			<span class="text-gray-600">Last:</span>
			<A href={linkToolGithub} rel="noopener noreferrer">
				{lastCommit}
			</A>
			{lastDate}
		</div>
	</div>
	<Tooltip type="auto">
		{`Found ${errorStat.overall} times across ${errorStat.test_paths_count} files over ${totalRuns} runs. On average ${errorsPerFailedTest} errors per failing file. The most recent occurrence was in commit ${lastCommit} on ${lastDate}`}
	</Tooltip>
{/if}
