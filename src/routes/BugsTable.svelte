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
		Badge
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
	import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';
	import { allFoundErrors } from '$lib/generated/errors_data';
	import { formatDateDMY, getFirstFound, displayIssueNovelty } from '$lib/index';
	import { page } from '$app/state';
	import { goto } from '$app/navigation';
	import { onMount } from 'svelte';
	import { browser } from '$app/environment';
	import { LinkHandler } from '$lib/index';

	let toolChoices: CheckBoxChoice[] = Array.from(new Set(allFoundErrors.map((i) => i.tool)))
		.sort()
		.map((tool) => ({ value: tool, label: tool }));

	// Novelty filter choices
	let noveltyChoices: CheckBoxChoice[] = Array.from(
		new Set(allFoundErrors.map((i) => i.issue_novelty))
	)
		.filter((n) => n !== null)
		.map((novelty) => ({
			value: novelty!,
			label: displayIssueNovelty(novelty!)
		}))
		.sort((a, b) => a.label.localeCompare(b.label));
	let noveltyGroup: string[] = [];

	// Maintainers response filter choices
	let maintainersChoices: CheckBoxChoice[] = Array.from(
		new Set(allFoundErrors.map((i) => i.maintainers_response))
	)
		.filter((r) => r !== null)
		.map((response) => ({
			value: response!,
			label: getMaintainersResponseDisplay(response!).text
		}))
		.sort((a, b) => a.label.localeCompare(b.label));
	let maintainersGroup: string[] = [];
	let toolGroup: string[] = [];
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
		const sortParam = url.searchParams.get('sort');
		if (sortParam && ['firstFound', 'title'].includes(sortParam)) {
			sortColumn = sortParam as SortableColumn;
		}
		const ascParam = url.searchParams.get('asc');
		sortAsc = ascParam === '0' ? false : true;
	}

	onMount(() => {
		loadFromQueryParams(page.url);
	});

	$: toolFilteredErrors =
		toolGroup.length > 0
			? allFoundErrors.filter((e) => toolGroup.includes(e.tool))
			: allFoundErrors;

	$: noveltyFilteredErrors =
		noveltyGroup.length > 0
			? toolFilteredErrors.filter((e) => e.issue_novelty && noveltyGroup.includes(e.issue_novelty))
			: toolFilteredErrors;

	$: maintainersFilteredErrors =
		maintainersGroup.length > 0
			? noveltyFilteredErrors.filter(
					(e) => e.maintainers_response && maintainersGroup.includes(e.maintainers_response)
				)
			: noveltyFilteredErrors;

	$: sortedErrors = [...maintainersFilteredErrors].sort((a, b) => {
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

		// Check if it's a comment link
		if (link.includes('#issuecomment-')) {
			return `Comment on #${issueNumber}`;
		}

		// Regular issue link
		return `Issue #${issueNumber}`;
	}

	function getMaintainersResponseDisplay(tag: MaintainersResponse) {
		switch (tag) {
			case 'bug':
				return { text: 'Bug', color: 'red' as const };
			case 'enhancement':
				return { text: 'Enhancement', color: 'blue' as const };
			case 'low':
				return { text: 'Low priority', color: 'purple' as const };
			case 'wontfix':
				return { text: "Won't fix", color: 'gray' as const };
			default:
				return { text: 'No response', color: 'green' as const };
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
	$: if (browser && toolGroup) updateQueryParams();
	$: if (browser && noveltyGroup) updateQueryParams();
	$: if (browser && maintainersGroup) updateQueryParams();
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
	</div>
	<Table hoverable={true} class="mt-6 min-w-full divide-y divide-gray-200 dark:divide-gray-600">
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
				widthClass="px-4 py-3 w-64"
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
			<TableHeadCellSortable label="Stage" widthClass="" extraClass="text-center text-gray-700" />
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
			<TableHeadCellSortable label="Link" widthClass="" extraClass="text-center text-gray-700" />
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
					<TableBodyCell class="w-12 px-4 py-3 text-center text-gray-400">{i + 1}</TableBodyCell>
					<TableBodyCell class="w-64 px-4 py-3">
						<A href={LinkHandler(`/error/${item.id}`)}>{item.title}</A>
					</TableBodyCell>
					<TableBodyCell class="w-32 px-4 py-3 text-center">{item.tool}</TableBodyCell>
					<TableBodyCell class="w-32 px-4 py-3 text-center">{item.stage}</TableBodyCell>
					<TableBodyCell class="w-32 px-4 py-3 text-center"
						>{formatDateDMY(getFirstFound(item))}</TableBodyCell
					>
					<TableBodyCell class="w-32 px-4 py-3 text-center">
						{@const noveltyDisplay = displayIssueNovelty(item.issue_novelty)}
						{#if noveltyDisplay}
							<Badge color="gray" large>{noveltyDisplay}</Badge>
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
</Card>
