<script lang="ts">
	import { Card, Heading, A, Button } from 'flowbite-svelte';
	import TableColSortHead from '$lib/components/bugs-table/TableColSortHead.svelte';
	import TableColHead from '$lib/components/bugs-table/TableColHead.svelte';
	import TableColFilterHead from '$lib/components/bugs-table/TableColFilterHead.svelte';
	import TableData from '$lib/components/bugs-table/TableData.svelte';
	import BottomScrollbar from '$lib/components/bugs-table/BottomScrollbar.svelte';
	import { type CheckBoxChoice, type SortableColumn, type SortDirection, type FoundError } from '$lib/core';
	import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';
	import { allFoundErrors } from '$lib/generated/errors-data';
	import { formatDateDMY, getFirstFound } from '$lib/index';
	import {
		createToolChoices,
		createStageChoices,
		createNoveltyChoices,
		createMaintainersChoices,
		createIssueTypeChoices,
		applyFilters,
		createFilter
	} from '$lib/components/bugs-table/table-utils';
	import { SortedIssues } from '$lib/components/bugs-table/bugs-table-utils';
	import { page } from '$app/state';
	import { goto } from '$app/navigation';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { fixLink, getMaintainersResponseDisplay } from '$lib/index';
	import { errorsStats } from '$lib/parsed-error-stats';
	import ErrorStatsCell from '$lib/components/bugs-table/ErrorStatsCell.svelte';
	import IssueLinksCell from '$lib/components/bugs-table/IssueLinksCell.svelte';
	import { errorPercentages } from '$lib/components/bugs-table/error-stats-utils';
	import IssueTypeBadges from '$lib/components/IssueTypeBadges.svelte';
	import NoveltyBadge from '$lib/components/NoveltyBadge.svelte';
	import MaintainersResponseBadge from '$lib/components/MaintainersResponseBadge.svelte';

	let scrollContainer: HTMLDivElement;

	let toolChoices: CheckBoxChoice[] = createToolChoices(allFoundErrors);
	let stageChoices: CheckBoxChoice[] = createStageChoices(allFoundErrors);
	let noveltyChoices: CheckBoxChoice[] = createNoveltyChoices(allFoundErrors);
	let maintainersChoices: CheckBoxChoice[] = createMaintainersChoices(
		allFoundErrors,
		getMaintainersResponseDisplay
	);
	let issueTypeChoices: CheckBoxChoice[] = createIssueTypeChoices(allFoundErrors);

	let toolGroup: string[] = [];
	let stageGroup: string[] = [];
	let issueTypeGroup: string[] = [];
	let noveltyGroup: string[] = [];
	let maintainersGroup: string[] = [];

	const filterDefs: Array<{
		param: string;
		get: () => string[];
		set: (v: string[]) => void;
		getValue: (e: FoundError) => any;
	}> = [
		{ param: 'tools', get: () => toolGroup, set: (v) => (toolGroup = v), getValue: (e) => e.tool },
		{ param: 'stage', get: () => stageGroup, set: (v) => (stageGroup = v), getValue: (e) => e.stage },
		{ param: 'issue_type', get: () => issueTypeGroup, set: (v) => (issueTypeGroup = v), getValue: (e) => e.issue_type },
		{ param: 'novelty', get: () => noveltyGroup, set: (v) => (noveltyGroup = v), getValue: (e) => e.issue_novelty },
		{ param: 'maintainers', get: () => maintainersGroup, set: (v) => (maintainersGroup = v), getValue: (e) => e.maintainers_response }
	];

	let sortColumn: SortableColumn = 'title';
	let sortDest: SortDirection = 'asc';

	function updateQueryParams() {
		if (!browser) return;
		const params = new URLSearchParams();
		for (const f of filterDefs) {
			const values = f.get();
			if (values.length > 0) params.set(f.param, values.join(','));
		}
		if (sortColumn) params.set('sort', sortColumn);
		params.set('dir', sortDest);
		goto(`?${params.toString()}`, { replaceState: true, keepFocus: true, noScroll: true });
	}

	function loadFromQueryParams(url: URL) {
		for (const f of filterDefs) {
			const p = url.searchParams.get(f.param);
			f.set(p ? p.split(',') : []);
		}
		const sortParam = url.searchParams.get('sort');
		if (sortParam && ['firstFound', 'title', 'stats'].includes(sortParam)) {
			sortColumn = sortParam as SortableColumn;
		}
		const dirParam = url.searchParams.get('dir');
		sortDest = dirParam === 'desc' ? 'desc' : 'asc';
	}

	function clearAllFilters() {
		for (const f of filterDefs) f.set([]);
		if (browser) updateQueryParams();
	}

	onMount(() => {
		loadFromQueryParams(page.url);
	});
	$: filteredErrors = applyFilters(allFoundErrors, [
		createFilter(toolGroup, (e) => e.tool),
		createFilter(stageGroup, (e) => e.stage),
		createFilter(issueTypeGroup, (e) => e.issue_type),
		createFilter(noveltyGroup, (e) => e.issue_novelty),
		createFilter(maintainersGroup, (e) => e.maintainers_response)
	]);

	$: sortedErrors = new SortedIssues(filteredErrors, errorPercentages).sorted(sortColumn, sortDest);

	function setSort(col: SortableColumn) {
		if (sortColumn === col) {
			sortDest = sortDest === 'asc' ? 'desc' : 'asc';
		} else {
			sortColumn = col;
			sortDest = 'asc';
		}
		if (browser) updateQueryParams();
	}

	$: if (browser && filteredErrors) updateQueryParams();
</script>

<Card size="xl" class="max-w-none p-4 shadow-sm sm:p-6">
	<div class="items-center justify-between lg:flex">
		<div class="mt-px mb-4 lg:mb-0">
			<Heading tag="h3" class="mb-2 -ml-0.25 text-xl font-semibold dark:text-white"
				>Found bugs & issues</Heading
			>
			<p class="text-base font-normal text-gray-500 dark:text-gray-300">
				This is a list of bugs and issues found by
				<A href={githubUrl} class="underline">verilog-model</A> using
				<A href={depTyCheckGithubUrl} class="underline">DepTyCheck</A> for various HDL analysis tools.
				<br />
				There are examples that reproduce these bugs.
			</p>
		</div>
		<div class="flex items-center gap-2">
			<Button color="light" onclick={clearAllFilters}>Clear filters</Button>
		</div>
	</div>
	<div class="relative mt-6">
		<p class="mb-2 text-sm dark:text-white">{sortedErrors.length} issues</p>
		<div bind:this={scrollContainer} style="overflow-x: auto; max-width: 100%;">
			<table class="w-full min-w-max divide-y divide-gray-200 text-sm dark:divide-gray-600">
				<thead class="bg-gray-50 dark:bg-gray-700">
					<tr>
						<TableColHead label="№" widthClass="w-12" colorClass="text-gray-400" />
						<TableColSortHead label="Title" sortKey="title" {sortColumn} {sortDest} {setSort} />
						<TableColFilterHead
							choices={toolChoices}
							bind:group={toolGroup}
							label="Tool"
							name="tools"
						/>
						<TableColSortHead
							label="First Found"
							sortKey="firstFound"
							{sortColumn}
							{sortDest}
							{setSort}
						/>
						<TableColFilterHead
							choices={issueTypeChoices}
							bind:group={issueTypeGroup}
							label="Issue type"
							name="issue_type"
						/>
						<TableColFilterHead
							choices={stageChoices}
							bind:group={stageGroup}
							label="Stage"
							name="stage"
						/>
						<TableColFilterHead
							choices={noveltyChoices}
							bind:group={noveltyGroup}
							label="Novelty"
							name="novelty"
						/>
						<TableColHead label="Related<br>issues" />
						<TableColFilterHead
							choices={maintainersChoices}
							bind:group={maintainersGroup}
							label="Maintainers<br>response"
							name="maintainers"
						/>
						<TableColSortHead
							label="Stats"
							sortKey="stats"
							{sortColumn}
							{sortDest}
							{setSort}
							hint="% of all runs / % of failed files / errors per failing file"
						/>
					</tr>
				</thead>
				<tbody class="divide-y divide-gray-200 bg-white dark:divide-gray-600 dark:bg-gray-800">
					{#each sortedErrors as item, i (item.id)}
						<tr class="hover:bg-gray-50 dark:hover:bg-gray-700">
							<TableData widthClass="w-12" specialClass="text-gray-400">{i + 1}</TableData>
							<TableData textAlign="text-left">
								<A
									href={fixLink(`/error/${item.id}`)}
									class="line-clamp-2 break-words whitespace-normal"
								>
									{item.title}
								</A>
							</TableData>
							<TableData>{item.tool}</TableData>
							<TableData>{formatDateDMY(getFirstFound(item))}</TableData>
							<TableData>
								<IssueTypeBadges types={item.issue_type} />
							</TableData>
							<TableData>{item.stage}</TableData>
							<TableData>
								<NoveltyBadge novelty={item.issue_novelty} />
							</TableData>
							<TableData>
								<IssueLinksCell issueLinks={item.issue_links} />
							</TableData>
							<TableData>
								{#if item.maintainers_response}
									<MaintainersResponseBadge response={item.maintainers_response} />
								{/if}
							</TableData>
							<TableData widthClass="w-48">
								{#if errorsStats.errors[item.id]}
									<ErrorStatsCell
										errorId={item.id}
										{errorsStats}
										percentages={errorPercentages[item.id]}
									/>
								{/if}
							</TableData>
						</tr>
					{/each}
				</tbody>
			</table>
		</div>
	</div>
</Card>

<div class="h-12"></div>

<BottomScrollbar {scrollContainer} />
