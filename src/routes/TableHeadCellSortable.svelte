<script lang="ts">
	import { TableHeadCell } from 'flowbite-svelte';
	import type { SortableColumn } from '$lib/core';
	import { CaretSortOutline, CaretUpSolid, CaretDownSolid } from 'flowbite-svelte-icons';
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
		{scope}
		class={`${widthClass} ${extraClass}`}
		onclick={() => setSort(sortKey)}
		style="cursor:pointer"
	>
		<span class="whitespace-nowrap">
			{@html label}
			{#if sortKey && sortColumn === sortKey}
				{#if sortAsc}
					<CaretUpSolid class="ml-1 inline-block h-4 w-4 align-middle" />
				{:else}
					<CaretDownSolid class="ml-1 inline-block h-4 w-4 align-middle" />
				{/if}
			{:else}
				<CaretSortOutline class="ml-1 inline-block h-4 w-4 align-middle text-gray-400" />
			{/if}
		</span>
		<slot />
	</TableHeadCell>
{:else}
	<TableHeadCell {scope} class={`${widthClass} ${extraClass}`}>
		<span class="whitespace-nowrap">
			{@html label}
		</span>
		<slot />
	</TableHeadCell>
{/if}
