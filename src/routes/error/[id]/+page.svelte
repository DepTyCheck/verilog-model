<script lang="ts">
	import { page } from '$app/state';
	import { allFoundErrors } from '$lib/generated/errors-data';
	import type { FoundError } from '$lib/core';
	import { defaultMargin } from '$lib/styles';
	import { formatDateDMY, getFirstFound } from '$lib/index';
	import VCard from '$lib/components/full-issue-info/VCard.svelte';
	import CodeBlock from '$lib/components/full-issue-info/CodeBlock.svelte';
	import FieldDisplay from '$lib/components/full-issue-info/FieldDisplay.svelte';
	import IssueLinksCell from '$lib/components/bugs-table/IssueLinksCell.svelte';
	import IssueTypeBadges from '$lib/components/IssueTypeBadges.svelte';
	import NoveltyBadge from '$lib/components/NoveltyBadge.svelte';
	import MaintainersResponseBadge from '$lib/components/MaintainersResponseBadge.svelte';
	import ErrorStatsCell from '$lib/components/bugs-table/ErrorStatsCell.svelte';
	import { errorsStats } from '$lib/parsed-error-stats';
	import { errorPercentages } from '$lib/components/bugs-table/error-stats-utils';

	let id: string = String(page.params.id);
	let foundError: FoundError | null = null;
	let errorMsg: string | null = '';

	foundError = allFoundErrors.find((e) => e.id === id) ?? null;
	if (!foundError) {
		errorMsg = `No foundError found for id: ${id}`;
	}
</script>

<div class={defaultMargin}>
	{#if errorMsg}
		<div class="mb-4 rounded bg-red-100 p-4 text-red-800">{errorMsg}</div>
	{:else if foundError}
		<h1 class="mb-4 text-2xl font-semibold dark:text-white">{foundError.title}</h1>
		<dl class="mb-8 divide-y divide-gray-100 border-y border-gray-100 dark:divide-gray-800 dark:border-gray-800">
			{#if foundError.tool}
				<FieldDisplay label="Tool" value={foundError.tool} />
			{/if}
			{#if foundError.stage}
				<FieldDisplay label="Stage" value={foundError.stage} />
			{/if}
			{#if getFirstFound(foundError)}
				<FieldDisplay label="First found" value={formatDateDMY(getFirstFound(foundError))} />
			{/if}
			{#if foundError.issue_type && foundError.issue_type.length > 0}
				<FieldDisplay label="Issue type">
					<IssueTypeBadges types={foundError.issue_type} />
				</FieldDisplay>
			{/if}
			{#if foundError.issue_novelty}
				<FieldDisplay label="Novelty">
					<NoveltyBadge novelty={foundError.issue_novelty} />
				</FieldDisplay>
			{/if}
			{#if foundError.maintainers_response}
				<FieldDisplay label="Maintainers response">
					<MaintainersResponseBadge response={foundError.maintainers_response} />
				</FieldDisplay>
			{/if}
			{#if errorsStats.errors[foundError.id]}
				<FieldDisplay label="Stats">
					<ErrorStatsCell
						errorId={foundError.id}
						{errorsStats}
						percentages={errorPercentages[foundError.id]}
					/>
				</FieldDisplay>
			{/if}
			{#if foundError.issue_links && foundError.issue_links.length > 0}
				<FieldDisplay label="Related issues">
					<IssueLinksCell issueLinks={foundError.issue_links} />
				</FieldDisplay>
			{/if}
			{#if foundError.short_desc}
				<FieldDisplay label="Description" value={foundError.short_desc} />
			{/if}
		</dl>
		<h2 class="mt-6 mb-2 text-lg font-semibold dark:text-white">Examples:</h2>
		{#each foundError.examples as example}
			<div class="mb-6">
				<VCard heading="" info="">
					{#if example.minified_error != null && example.minified_error !== ''}
						<h3 class="mt-2 mb-1 font-semibold">Minified example error</h3>
						<CodeBlock code={example.minified_error} />
					{/if}
					{#if example.minified_example != null && example.minified_example !== ''}
						<h3 class="mt-4 mb-1 font-semibold">Minified example</h3>
						<CodeBlock code={example.minified_example} language="verilog" />
					{/if}
					{#if example.full_error != null && example.full_error !== ''}
						<h3 class="mt-4 mb-1 font-semibold">Generated example error</h3>
						<CodeBlock code={example.full_error} />
					{/if}
					{#if example.full_example != null && example.full_example !== ''}
						<h3 class="mt-4 mb-1 font-semibold">Generated example</h3>
						<CodeBlock code={example.full_example} language="verilog" />
					{/if}
				</VCard>
			</div>
		{/each}
	{/if}
</div>
