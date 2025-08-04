<script lang="ts">
    import { Button, Dropdown, Checkbox, TableBody, TableBodyCell, TableBodyRow, Table, TableHead, Card, Heading, A, Tooltip } from 'flowbite-svelte';
    import TableHeadCellSortable from './TableHeadCellSortable.svelte';
    import { FilterSolid, ArrowRightOutline } from 'flowbite-svelte-icons';
    import { type CheckBoxChoice, type SortableColumn, type IssueStatus } from '$lib/core';
    import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';
    import { allFoundErrors } from '$lib/generated/errors_data';
    import { formatDateDMY, getFirstFound, displayIssueStatus } from '$lib/index';
    import { page } from '$app/state';
    import { goto } from '$app/navigation';
    import { onMount } from 'svelte';
    import { browser } from '$app/environment';
    import { LinkHandler } from '$lib/index';
  
    let toolChoices: CheckBoxChoice[] = Array.from(new Set(allFoundErrors.map(i => i.tool)))
          .sort()
          .map(tool => ({ value: tool, label: tool }));
    let group: string[] = [];
    let sortColumn: SortableColumn = 'title';
    let sortAsc = true;

    function getStatusTooltip(status: IssueStatus | undefined): string {
      switch (status) {
        case 'reported':
          return "This issue is reported for the first time";
        case 'already_known':
          return 'This issue was already known before';
        case 'unsupported':
          return 'This feature is not supported by the tool and is not planned in the near future';
        case null:
          return 'No status information available';
        default:
          return `Status: ${status}`;
      }
    }

    function updateQueryParams() {
      if (!browser) return;
      const params = new URLSearchParams();
      if (group.length > 0) params.set('tools', group.join(','));
      if (sortColumn) params.set('sort', sortColumn);
      params.set('asc', sortAsc ? '1' : '0');
      goto(`?${params.toString()}`, { replaceState: true, keepFocus: true, noScroll: true });
    }

    function loadFromQueryParams(url: URL) {
      const toolsParam = url.searchParams.get('tools');
      group = toolsParam ? toolsParam.split(',') : [];
      const sortParam = url.searchParams.get('sort');
      if (sortParam && ['tool','firstFound','title','stage','issue_link','issue_status'].includes(sortParam)) {
        sortColumn = sortParam as SortableColumn;
      }
      const ascParam = url.searchParams.get('asc');
      sortAsc = ascParam === '0' ? false : true;
    }

    onMount(() => {
      loadFromQueryParams(page.url);
    });

    $: filteredErrors = group.length > 0
      ? allFoundErrors.filter(e => group.includes(e.tool))
      : allFoundErrors;
  
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
      const match = link.match(/\/issues\/(\d+)/);
      return match ? match[1] : null;
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

    // Update query params when filter changes
    $: if (browser && group) updateQueryParams();
  </script>
  
  <Card size="xl" class="max-w-none p-4 shadow-sm sm:p-6">
    <div class="items-center justify-between lg:flex">
      <div class="mt-px mb-4 lg:mb-0">
        <Heading tag="h3" class="mb-2 -ml-0.25 text-xl font-semibold dark:text-white">Found bugs</Heading>
        <p class="text-base font-normal text-gray-500 dark:text-gray-300">
          This is a list of bugs and issues found by 
          <A href={githubUrl} class="underline">verilog-model</A> using
          <A href={depTyCheckGithubUrl} class="underline">DepTyCheck</A> for various SystemVerilog tools.
          <br/>
          There are examples that reproduce these bugs.
        </p>
      </div>
      <div class="items-center justify-between gap-3 space-y-4 sm:flex sm:space-y-0">
        <div class="flex items-center">
          <Button color="alternative" class="w-fit px-4 py-2 whitespace-nowrap">
            Filter Tools <FilterSolid class="ml-2 h-4 w-4" />
          </Button>
          <Dropdown simple class="w-48 space-y-2 text-sm p-3">
            <div class="flex flex-col gap-2">
              <Checkbox name="tools" choices={toolChoices} bind:group />
            </div>
          </Dropdown>
        </div>
      </div>
    </div>
    <Table hoverable={true} class="mt-6 min-w-full divide-y divide-gray-200 dark:divide-gray-600">
      <TableHead>
        <TableHeadCellSortable
          label="№"
          widthClass="px-4 py-3 w-12"
          extraClass="text-gray-400"
        />
        <TableHeadCellSortable
          label="Title"
          sortKey="title"
          {sortColumn}
          {sortAsc}
          {setSort}
          widthClass="px-4 py-3 w-64"
        />
        <TableHeadCellSortable
          label="Tool"
          sortKey="tool"
          {sortColumn}
          {sortAsc}
          {setSort}
        />
        <TableHeadCellSortable
          label="Stage"
          sortKey="stage"
          {sortColumn}
          {sortAsc}
          {setSort}
        />
        <TableHeadCellSortable
          label="First Found"
          sortKey="firstFound"
          {sortColumn}
          {sortAsc}
          {setSort}
        />
        <TableHeadCellSortable
          label="Status"
          sortKey="issue_status"
          {sortColumn}
          {sortAsc}
          {setSort}
        />
        <TableHeadCellSortable
          label="Link"
          sortKey="issue_link"
          {sortColumn}
          {sortAsc}
          {setSort}
        />
      </TableHead>
      <TableBody>
        {#each sortedErrors as item, i}
          <TableBodyRow>
            <TableBodyCell class="px-4 py-3 w-12 text-gray-400">{i + 1}</TableBodyCell>
            <TableBodyCell class="px-4 py-3 w-64">
              <A href={LinkHandler(`/error/${item.id}`)}>{item.title}</A>
            </TableBodyCell>
            <TableBodyCell class="px-4 py-3 w-32">{item.tool}</TableBodyCell>
            <TableBodyCell class="px-4 py-3 w-32">{item.stage}</TableBodyCell>
            <TableBodyCell class="px-4 py-3 w-32">{formatDateDMY(getFirstFound(item))}</TableBodyCell>
            <TableBodyCell class="px-4 py-3 w-32">
              {displayIssueStatus(item.issue_status)}
              <Tooltip>{getStatusTooltip(item.issue_status)}</Tooltip>
            </TableBodyCell>
            <TableBodyCell class="px-4 py-3 w-32">
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
          </TableBodyRow>
        {/each}
      </TableBody>
    </Table>
  </Card>
  