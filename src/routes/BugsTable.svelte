<script lang="ts">
	import {
		TableBody,
		TableBodyCell,
		TableBodyRow,
		Table,
		TableHead,
		Card,
		Heading,
		A,
		Tooltip,
		Badge,
		Button
	} from 'flowbite-svelte';
	import TableHeadCellSortable from './TableHeadCellSortable.svelte';
	import FilterButton from './FilterButton.svelte';
	import { ArrowRightOutline } from 'flowbite-svelte-icons';
	import {
		type CheckBoxChoice,
		type SortableColumn,
		type IssueNovelty,
		type MaintainersResponse
	} from '$lib/core';
	import type { DisplayInfo } from '$lib/index';
	import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';
	import { allFoundErrors } from '$lib/generated/errors_data';
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

	let scrollContainer: HTMLDivElement;
	let bottomScrollbar: HTMLInputElement;
	let showLeftShadow = false;
	let showRightShadow = false;
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
		if (sortParam && ['firstFound', 'title'].includes(sortParam)) {
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
		// Initialize shadows after first render
		requestAnimationFrame(() => {
			if (scrollContainer) updateShadows(scrollContainer);
		});
		// Update on resize as table width may change
		const onResize = () => {
			if (scrollContainer) updateShadows(scrollContainer);
		};
		window.addEventListener('resize', onResize);
		return () => window.removeEventListener('resize', onResize);
	});

	// Update scrollbar visibility when filtered data changes
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

	// Update query params when filter changes
	$: if (browser && filteredErrors) updateQueryParams();

	function updateShadows(el: HTMLElement | null) {
		if (!el) return;
		const maxScrollLeft = el.scrollWidth - el.clientWidth;
		const current = el.scrollLeft;
		showLeftShadow = current > 0;
		showRightShadow = current < maxScrollLeft - 1;
		showBottomScrollbar = maxScrollLeft > 0;
	}

	function handleScroll(e: any) {
		const el = e.target as HTMLElement;
		updateShadows(el);
		// Sync bottom scrollbar with table scroll
		if (bottomScrollbar) {
			const maxScroll = el.scrollWidth - el.clientWidth;
			const scrollPercentage = maxScroll > 0 ? (el.scrollLeft / maxScroll) * 100 : 0;
			bottomScrollbar.value = scrollPercentage.toString();
		}
	}

	function handleBottomScrollbarChange() {
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
			<Table
				hoverable={true}
				divClass=""
				class="min-w-max divide-y divide-gray-200 dark:divide-gray-600"
			>
				<TableHead>
					<TableHeadCellSortable
						label="№"
						widthClass="px-4 py-3 w-12"
						extraClass="text-gray-400 text-center"
					/>
					<TableHeadCellSortable
						label="Title"
						sortKey="title"
						{sortColumn}
						{sortAsc}
						{setSort}
						widthClass="px-4 py-3 w-32"
						extraClass="text-gray-700"
					/>
					<TableHeadCellSortable label="" widthClass="">
						<FilterButton
							choices={toolChoices}
							bind:group={toolGroup}
							label="Tool"
							name="tools"
							extraClass="text-gray-700"
						/>
					</TableHeadCellSortable>
					<TableHeadCellSortable label="" widthClass="px-4 py-3 w-32" extraClass="text-center">
						<FilterButton
							choices={stageChoices}
							bind:group={stageGroup}
							label="Stage"
							name="stage"
							extraClass="text-gray-700"
						/>
					</TableHeadCellSortable>
					<TableHeadCellSortable
						label="First Found"
						sortKey="firstFound"
						{sortColumn}
						{sortAsc}
						{setSort}
						extraClass="text-center text-gray-700"
					/>
					<TableHeadCellSortable label="" widthClass="" extraClass="text-center">
						<FilterButton
							choices={noveltyChoices}
							bind:group={noveltyGroup}
							label="Novelty"
							name="novelty"
							extraClass="text-gray-700"
						/>
					</TableHeadCellSortable>
					<TableHeadCellSortable
						label="Related<br>issue"
						widthClass=""
						extraClass="text-center text-gray-700"
					/>
					<TableHeadCellSortable label="" widthClass="px-4 py-3 w-32" extraClass="text-center">
						<FilterButton
							choices={maintainersChoices}
							bind:group={maintainersGroup}
							label="Maintainers<br>response"
							name="maintainers"
							extraClass="text-gray-700"
						/>
					</TableHeadCellSortable>
				</TableHead>
				<TableBody>
					{#each sortedErrors as item, i}
						<TableBodyRow>
							<TableBodyCell class="w-12 px-4 py-3 text-center text-gray-400">{i + 1}</TableBodyCell
							>
							<TableBodyCell class="h-16 w-32 px-4 py-3">
								<A
									href={LinkHandler(`/error/${item.id}`)}
									class="line-clamp-2 break-words whitespace-normal">{item.title}</A
								>
							</TableBodyCell>
							<TableBodyCell class="w-32 px-4 py-3 text-center">{item.tool}</TableBodyCell>
							<TableBodyCell class="w-32 px-4 py-3 text-center">{item.stage}</TableBodyCell>
							<TableBodyCell class="w-32 px-4 py-3 text-center"
								>{formatDateDMY(getFirstFound(item))}</TableBodyCell
							>
							<TableBodyCell class="w-32 px-4 py-3 text-center">
								{@const noveltyDisplay = displayIssueNovelty(item.issue_novelty)}
								{#if noveltyDisplay.text}
									<Badge color={noveltyDisplay.color} large>{noveltyDisplay.text}</Badge>
									<Tooltip type="auto">{getNoveltyTooltip(item.issue_novelty)}</Tooltip>
								{/if}
							</TableBodyCell>
							<TableBodyCell class="w-32 px-4 py-3 text-center">
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
							</TableBodyCell>
							<TableBodyCell class="w-32 px-4 py-3 text-center">
								{#if item.maintainers_response}
									{@const resp = getMaintainersResponseDisplay(item.maintainers_response)}
									<Badge color={resp.color} large rounded>{resp.text}</Badge>
									<Tooltip type="auto"
										>{getMaintainersResponseTooltip(item.maintainers_response)}</Tooltip
									>
								{/if}
							</TableBodyCell>
						</TableBodyRow>
					{/each}
				</TableBody>
			</Table>
		</div>
		<!-- {#if showLeftShadow}
			<div
				class="pointer-events-none absolute top-0 bottom-0 left-0 w-16
				rounded-l-md bg-gradient-to-r from-gray-200 to-transparent dark:from-gray-800"
			></div>
		{/if}

		{#if showRightShadow}
			<div
				class="pointer-events-none absolute top-0 right-0 bottom-0 w-16
				rounded-r-md bg-gradient-to-l from-gray-200 to-transparent dark:from-gray-800"
			></div>
		{/if} -->
	</div>
</Card>

<!-- Add bottom spacing to prevent overlap with floating scrollbar -->
<div class="h-12"></div>

<!-- Fixed bottom scrollbar for table horizontal scrolling -->
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
