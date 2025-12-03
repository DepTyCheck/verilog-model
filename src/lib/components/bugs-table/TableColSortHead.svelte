<script lang="ts">
	import type { SortableColumn } from '$lib/core';
	import { CaretSortOutline, CaretUpSolid, CaretDownSolid } from 'flowbite-svelte-icons';
	import { Button } from 'flowbite-svelte';
	import { tableHeaderText, tableHeaderColor } from './table-styles';
	import TableColHead from './TableColHead.svelte';

	export let label: string;
	export let sortKey: SortableColumn;
	export let sortColumn: SortableColumn | null = null;
	export let sortAsc: boolean = true;
	export let setSort: (col: SortableColumn) => void;
	export let scope: 'col' | 'colgroup' | 'row' | 'rowgroup' | null = 'col';
	export let widthClass: string = 'px-4 py-3 w-32';
	export let hint: string | undefined = undefined;
</script>

<TableColHead {scope} {widthClass}>
	<div class="flex flex-col items-center justify-center">
		<Button
			color="alternative"
			class="w-fit border-none px-4 py-2 whitespace-nowrap shadow-none {tableHeaderText} {tableHeaderColor}"
			style="border: none; box-shadow: none;"
			onclick={() => setSort(sortKey)}
		>
			{@html label}
			{#if sortKey && sortColumn === sortKey}
				{#if sortAsc}
					<CaretUpSolid class="ml-2 h-4 w-4" />
				{:else}
					<CaretDownSolid class="ml-2 h-4 w-4" />
				{/if}
			{:else}
				<CaretSortOutline class="ml-2 h-4 w-4 text-gray-400" />
			{/if}
		</Button>
		{#if hint}
			<div class="mt-1 text-center text-xs text-gray-500 dark:text-gray-400">
				{hint}
			</div>
		{/if}
		<slot />
	</div>
</TableColHead>
