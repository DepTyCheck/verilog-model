<script lang="ts">
	import { Card, Heading, A, Tooltip, Badge, Button } from 'flowbite-svelte';
	import TableColSortHead from '$lib/components/bugs-table/TableColSortHead.svelte';
	import TableColHead from '$lib/components/bugs-table/TableColHead.svelte';
	import TableColFilterHead from '$lib/components/bugs-table/TableColFilterHead.svelte';
	import TableData from '$lib/components/bugs-table/TableData.svelte';
	import BottomScrollbar from '$lib/components/bugs-table/BottomScrollbar.svelte';
	import { ArrowRightOutline } from 'flowbite-svelte-icons';
	import { type CheckBoxChoice, type SortableColumn } from '$lib/core';
	import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';
	import { allFoundErrors } from '$lib/generated/errors-data';
	import { formatDateDMY, getFirstFound, displayIssueNovelty } from '$lib/index';
	import {
		createToolChoices,
		createStageChoices,
		createNoveltyChoices,
		createMaintainersChoices,
		applyFilters,
		createToolFilter,
		createNoveltyFilter,
		createMaintainersFilter,
		createStageFilter
	} from '$lib/components/bugs-table/table-utils';
	import { ErrorsSorter } from '$lib/components/bugs-table/bugs-table-utils';
	import { page } from '$app/state';
	import { goto } from '$app/navigation';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import {
		fixLink,
		getMaintainersResponseDisplay,
		getMaintainersResponseTooltip,
		getNoveltyTooltip
	} from '$lib/index';
	import { errorsStats } from '$lib/parsed-error-stats';
	import ErrorStatsCell from '$lib/components/bugs-table/ErrorStatsCell.svelte';
	import { errorPercentages } from '$lib/components/bugs-table/error-stats-utils';

	let scrollContainer: HTMLDivElement;

	let toolChoices: CheckBoxChoice[] = createToolChoices(allFoundErrors);
	let stageChoices: CheckBoxChoice[] = createStageChoices(allFoundErrors);
	let noveltyChoices: CheckBoxChoice[] = createNoveltyChoices(allFoundErrors);
	let maintainersChoices: CheckBoxChoice[] = createMaintainersChoices(
		allFoundErrors,
		getMaintainersResponseDisplay
	);

	let noveltyGroup: string[] = [];
	let maintainersGroup: string[] = [];
	let toolGroup: string[] = [];
	let stageGroup: string[] = [];

	let sortColumn: SortableColumn = 'title';
	let sortAsc = true;

	function parseGitHubUrl(
		url: string
	): { owner: string; repo: string; issueNumber: string } | null {
		const match = url.match(/github\.com\/([^\/]+)\/([^\/]+)\/issues\/(\d+)/);
		if (!match) return null;
		return {
			owner: match[1],
			repo: match[2],
			issueNumber: match[3]
		};
	}

	function updateQueryParams() {
		if (!browser) return;
		const params = new URLSearchParams();
		if (toolGroup.length > 0) params.set('tools', toolGroup.join(','));
		if (noveltyGroup.length > 0) params.set('novelty', noveltyGroup.join(','));
		if (maintainersGroup.length > 0) params.set('maintainers', maintainersGroup.join(','));
		if (stageGroup.length > 0) params.set('stage', stageGroup.join(','));
		if (sortColumn) params.set('sort', sortColumn);
		params.set('asc', sortAsc ? '1' : '0');
		goto(`?${params.toString()}`, { replaceState: true, keepFocus: true, noScroll: true });
	}

	function loadFromQueryParams(url: URL) {
		const toolsParam = url.searchParams.get('tools');
		toolGroup = toolsParam ? toolsParam.split(',') : [];
		const noveltyParam = url.searchParams.get('novelty');
		noveltyGroup = noveltyParam ? noveltyParam.split(',') : [];
		const maintainersParam = url.searchParams.get('maintainers');
		maintainersGroup = maintainersParam ? maintainersParam.split(',') : [];
		const stageParam = url.searchParams.get('stage');
		stageGroup = stageParam ? stageParam.split(',') : [];
		const sortParam = url.searchParams.get('sort');
		if (sortParam && ['firstFound', 'title', 'stats'].includes(sortParam)) {
			sortColumn = sortParam as SortableColumn;
		}
		const ascParam = url.searchParams.get('asc');
		sortAsc = ascParam === '0' ? false : true;
	}

	function clearAllFilters() {
		toolGroup = [];
		noveltyGroup = [];
		maintainersGroup = [];
		stageGroup = [];
		if (browser) updateQueryParams();
	}

	onMount(() => {
		loadFromQueryParams(page.url);
	});
	$: filteredErrors = applyFilters(allFoundErrors, [
		createToolFilter(toolGroup),
		createNoveltyFilter(noveltyGroup),
		createMaintainersFilter(maintainersGroup),
		createStageFilter(stageGroup)
	]);

	$: sortedErrors = new ErrorsSorter(filteredErrors, errorPercentages).sorted(sortColumn, sortAsc);

	function setSort(col: SortableColumn) {
		if (sortColumn === col) {
			sortAsc = !sortAsc;
		} else {
			sortColumn = col;
			sortAsc = true;
		}
		if (browser) updateQueryParams();
	}

	function getIssueNumberFromLink(link: string | null | undefined): string | null {
		if (!link) return null;
		const parsed = parseGitHubUrl(link);
		return parsed && parsed.issueNumber ? String(parsed.issueNumber) : null;
	}

	function formatLinkText(link: string | null | undefined): string {
		if (!link) return '';

		const issueNumber = getIssueNumberFromLink(link);
		if (!issueNumber) return '';

		return `#${issueNumber}`;
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
				<A href={depTyCheckGithubUrl} class="underline">DepTyCheck</A> for various SystemVerilog tools.
				<br />
				There are examples that reproduce these bugs.
			</p>
		</div>
		<div class="flex items-center gap-2">
			<Button color="light" onclick={clearAllFilters}>Clear filters</Button>
		</div>
	</div>
	<div class="relative mt-6">
		<div bind:this={scrollContainer} style="overflow-x: auto; max-width: 100%;">
			<table class="w-full min-w-max divide-y divide-gray-200 text-sm dark:divide-gray-600">
				<thead class="bg-gray-50 dark:bg-gray-700">
					<tr>
						<TableColHead label="№" widthClass="w-12" colorClass="text-gray-400" />
						<TableColSortHead label="Title" sortKey="title" {sortColumn} {sortAsc} {setSort} />
						<TableColFilterHead
							choices={toolChoices}
							bind:group={toolGroup}
							label="Tool"
							name="tools"
						/>
						<TableColFilterHead
							choices={stageChoices}
							bind:group={stageGroup}
							label="Stage"
							name="stage"
						/>
						<TableColSortHead
							label="First Found"
							sortKey="firstFound"
							{sortColumn}
							{sortAsc}
							{setSort}
						/>
						<TableColFilterHead
							choices={noveltyChoices}
							bind:group={noveltyGroup}
							label="Novelty"
							name="novelty"
						/>
						<TableColHead label="Related<br>issue" />
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
							{sortAsc}
							{setSort}
							hint="% of all runs / % of failed files / errors per failed file"
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
							<TableData>{item.stage}</TableData>
							<TableData>{formatDateDMY(getFirstFound(item))}</TableData>
							<TableData>
								{#if displayIssueNovelty(item.issue_novelty).text}
									{@const noveltyDisplay = displayIssueNovelty(item.issue_novelty)}
									<Badge color={noveltyDisplay.color} large>{noveltyDisplay.text}</Badge>
									<Tooltip type="auto">{getNoveltyTooltip(item.issue_novelty)}</Tooltip>
								{/if}
							</TableData>
							<TableData>
								{#if item.issue_link && item.issue_link.trim() !== ''}
									{#if getIssueNumberFromLink(item.issue_link)}
										<A href={item.issue_link} target="_blank" rel="noopener noreferrer">
											{formatLinkText(item.issue_link)}
										</A>
									{:else}
										<A href={item.issue_link} target="_blank" rel="noopener noreferrer">
											<ArrowRightOutline class="ms-1 h-5 w-5" />
										</A>
									{/if}
								{/if}
							</TableData>
							<TableData>
								{#if item.maintainers_response}
									{@const resp = getMaintainersResponseDisplay(item.maintainers_response)}
									<Badge color={resp.color} large rounded>{resp.text}</Badge>
									<Tooltip type="auto"
										>{getMaintainersResponseTooltip(item.maintainers_response)}</Tooltip
									>
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
