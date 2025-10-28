<script lang="ts">
	import {
		TableBody,
		TableBodyCell,
		TableBodyRow,
		Table,
		Card,
		Heading,
		A,
		Tooltip,
		Badge,
		Button
	} from 'flowbite-svelte';
	import TableColSortHead from '$lib/components/bugs-table/TableColSortHead.svelte';
	import TableColHead from '$lib/components/bugs-table/TableColHead.svelte';
	import TableColFilterHead from '$lib/components/bugs-table/TableColFilterHead.svelte';
	import { ArrowRightOutline } from 'flowbite-svelte-icons';
	import {
		type CheckBoxChoice,
		type SortableColumn,
		type IssueNovelty,
		type MaintainersResponse
	} from '$lib/core';
	import type { DisplayInfo } from '$lib/index';
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
	} from '$lib/table-utils';
	import { page } from '$app/state';
	import { goto } from '$app/navigation';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { LinkHandler } from '$lib/index';
	// import { getProcessedErrorsData } from '$lib/errors-utils';
	// import ErrorStatsCell from '$lib/components/bugs-table/ErrorStatsCell.svelte';

	// const errorsData = getProcessedErrorsData();

	let scrollContainer: HTMLDivElement;
	let bottomScrollbar: HTMLInputElement;
	let showBottomScrollbar = false;

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

	function getNoveltyTooltip(status: IssueNovelty): string {
		switch (status) {
			case 'new':
				return 'This issue is reported for the first time';
			case 'already_known':
				return 'This issue was already known before';
			case 'unsupported':
				return 'This feature is not supported by the tool and is not planned in the near future';
			case 'feature':
				return 'Actually it is not a bug, but a feature';
			case 'late':
				return 'An issue was detected, but the maintainers resolved it before it was reported';
			case null:
				return 'No novelty information available';
			default:
				return `Novelty: ${status}`;
		}
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
		requestAnimationFrame(() => {
			if (scrollContainer) updateShadows(scrollContainer);
		});
		const onResize = () => {
	
				if (scrollContainer) updateShadows(scrollContainer);
		
		};
		window.addEventListener('resize', onResize);
		return () => window.removeEventListener('resize', onResize);
	});

	$: if (scrollContainer) {
		requestAnimationFrame(() => {
			updateShadows(scrollContainer);
		});
	}

	$: filteredErrors = applyFilters(allFoundErrors, [
		createToolFilter(toolGroup),
		createNoveltyFilter(noveltyGroup),
		createMaintainersFilter(maintainersGroup),
		createStageFilter(stageGroup)
	]);

	$: sortedErrors = [...filteredErrors].sort((a, b) => {
		let aVal, bVal;
		if (sortColumn === 'firstFound') {
			aVal = a ? (getFirstFound(a)?.getTime() ?? 0) : 0;
			bVal = b ? (getFirstFound(b)?.getTime() ?? 0) : 0;
		} else if (sortColumn === 'stats') {
			// Sort by percentage occurrence
			// const aStat = errorsData.errors[a.id];
			// const bStat = errorsData.errors[b.id];
			aVal = 1;
			bVal = 2;
			// aVal = aStat && totalRuns > 0 ? (aStat.count / totalRuns) * 100 : 0;
			// bVal = bStat && totalRuns > 0 ? (bStat.count / totalRuns) * 100 : 0;
		} else {
			aVal = a[sortColumn] || '';
			bVal = b[sortColumn] || '';
		}
		if (aVal < bVal) return sortAsc ? -1 : 1;
		if (aVal > bVal) return sortAsc ? 1 : -1;
		return 0;
	});

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

	function getMaintainersResponseDisplay(tag: MaintainersResponse): DisplayInfo {
		switch (tag) {
			case 'bug':
				return { text: 'Bug', color: 'red' };
			case 'enhancement':
				return { text: 'Enhancement', color: 'blue' };
			case 'low':
				return { text: 'Low priority', color: 'purple' };
			case 'wontfix':
				return { text: "Won't fix", color: 'gray' };
			default:
				return { text: 'No response', color: 'green' };
		}
	}

	function getMaintainersResponseTooltip(status: MaintainersResponse): string {
		switch (status) {
			case 'bug':
				return 'The maintainers have accepted this bug';
			case 'enhancement':
				return "This feature isn't implemented yet, but it's planned";
			case 'low':
				return 'The maintainers are unsure whether this feature is needed';
			case 'wontfix':
				return 'There are no plans to implement this feature';
			case null:
				return 'No response information available';
			default:
				return status;
		}
	}

	$: if (browser && filteredErrors) updateQueryParams();

	function updateShadows(el: HTMLElement | null) {
		if (!el) return;
		
		showBottomScrollbar = el.scrollWidth > el.clientWidth;
	}

	function handleScroll(e: any) {
		const el = e.target as HTMLElement;
		updateShadows(el);

		// Update bottom scrollbar position
		// const bottomScrollbar = document.getElementById('bottom-scrollbar') as HTMLInputElement;
		if (bottomScrollbar) {
			const maxScroll = el.scrollWidth - el.clientWidth;
			const scrollPercentage = maxScroll > 0 ? (el.scrollLeft / maxScroll) * 100 : 0;
			bottomScrollbar.value = scrollPercentage.toString();
		}
	}

	function handleBottomScrollbarChange() {
		// const bottomScrollbar = document.getElementById('bottom-scrollbar') as HTMLInputElement;
		if (scrollContainer && bottomScrollbar) {
			const maxScroll = scrollContainer.scrollWidth - scrollContainer.clientWidth;
			const scrollLeft = (parseFloat(bottomScrollbar.value) / 100) * maxScroll;
			scrollContainer.scrollLeft = scrollLeft;
		}
	}

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
		<div
			bind:this={scrollContainer}
			on:scroll|passive={handleScroll}
			style="overflow-x: auto; max-width: 100%;"
		>
			<table class="min-w-max w-full divide-y divide-gray-200 dark:divide-gray-600 text-sm">
				<thead class="bg-gray-50 dark:bg-gray-700">
					<tr>
						<TableColHead
							label="№"
							widthClass="w-12"
							colorClass="text-gray-400"
						/>
						<TableColSortHead
							label="Title"
							sortKey="title"
							{sortColumn}
							{sortAsc}
							{setSort}
						/>
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
						<TableColHead
							label="Related<br>issue"
						/>
						<TableColFilterHead
							choices={maintainersChoices}
							bind:group={maintainersGroup}
							label="Maintainers<br>response"
							name="maintainers"
						/>
						<!-- <TableColSortHead
							label="Stats"
							sortKey="stats"
							{sortColumn}
							{sortAsc}
							{setSort}
						/> -->
					</tr>
				</thead>
				<tbody class="bg-white divide-y divide-gray-200 dark:bg-gray-800 dark:divide-gray-600">
					{#each sortedErrors as item, i}
						<tr class="hover:bg-gray-50 dark:hover:bg-gray-700">
							<td class="w-12 px-4 py-3 text-center text-gray-400">{i + 1}</td>
							<td class="h-16 w-32 px-4 py-3">
								<A
									href={LinkHandler(`/error/${item.id}`)}
									class="line-clamp-2 break-words whitespace-normal">{item.title}</A
								>
							</td>
							<td class="w-32 px-4 py-3 text-center">{item.tool}</td>
							<td class="w-32 px-4 py-3 text-center">{item.stage}</td>
							<td class="w-32 px-4 py-3 text-center">
								{formatDateDMY(getFirstFound(item))}
							</td>
							<td class="w-32 px-4 py-3 text-center">
								{#if displayIssueNovelty(item.issue_novelty).text}
									{@const noveltyDisplay = displayIssueNovelty(item.issue_novelty)}
									<Badge color={noveltyDisplay.color} large>{noveltyDisplay.text}</Badge>
									<Tooltip type="auto">{getNoveltyTooltip(item.issue_novelty)}</Tooltip>
								{/if}
							</td>
							<td class="w-32 px-4 py-3 text-center">
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
							</td>
							<td class="w-32 px-4 py-3 text-center">
								{#if item.maintainers_response}
									{@const resp = getMaintainersResponseDisplay(item.maintainers_response)}
									<Badge color={resp.color} large rounded>{resp.text}</Badge>
									<Tooltip type="auto"
										>{getMaintainersResponseTooltip(item.maintainers_response)}</Tooltip
									>
								{/if}
							</td>
							<!-- <td class="w-32 px-4 py-3 text-center">
								{#if errorsData.errors[item.id]}
									<ErrorStatsCell errorId={item.id} errorsStats={errorsData} />
								{/if}
							</td> -->
						</tr>
					{/each}
				</tbody>
			</table>
		</div>
	</div>
</Card>

<div class="h-12"></div>

{#if showBottomScrollbar}
	<div
		class="fixed right-8 bottom-4 left-8 z-50 rounded-xl border border-gray-200 bg-white/90 shadow-xl backdrop-blur-sm dark:border-gray-700 dark:bg-gray-900/90"
	>
		<div class="flex justify-center px-4 py-3">
			<div class="w-full max-w-4xl">
				<input
					bind:this={bottomScrollbar}
					type="range"
					min="0"
					max="100"
					value="0"
					on:input={handleBottomScrollbarChange}
					class="h-3 w-full cursor-pointer appearance-none rounded-lg bg-gray-200 dark:bg-gray-700"
				/>
			</div>
		</div>
	</div>
{/if}
