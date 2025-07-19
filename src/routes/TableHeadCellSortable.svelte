<script lang="ts">
  import { TableHeadCell } from 'flowbite-svelte';
  import type { SortableColumn } from '$lib/core';
  export let label: string;
  export let sortKey: SortableColumn | null = null;
  export let sortColumn: SortableColumn | null = null;
  export let sortAsc: boolean = true;
  export let setSort: ((col: SortableColumn) => void) | null = null;
  export let extraClass: string = '';
  export let scope: 'col' | 'colgroup' | 'row' | 'rowgroup' | null = 'col';
  export let widthClass: string = 'px-4 py-3 w-32';
</script>

{#if setSort && sortKey}
  <TableHeadCell
    scope={scope}
    class={`${widthClass} ${extraClass}`}
    onclick={() => setSort(sortKey)} 
    style="cursor:pointer"
  >
    {label}
    {#if sortKey && sortColumn === sortKey}
      {sortAsc ? '▲' : '▼'}
    {/if}
    <slot />
  </TableHeadCell>
{:else}
  <TableHeadCell
    {scope}
    class={`${widthClass} ${extraClass}`}
  >
    {label}
    <slot />
  </TableHeadCell>
{/if} 