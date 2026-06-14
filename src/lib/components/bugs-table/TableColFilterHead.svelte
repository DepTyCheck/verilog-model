<script lang="ts">
	import TableColHead from './TableColHead.svelte';
	import FilterButton from './FilterButton.svelte';
	import { Tooltip } from 'flowbite-svelte';
	import { type CheckBoxChoice } from '$lib/core';
	import { tableHeaderColor } from './table-styles';

	export let widthClass: string = 'px-4 py-3 w-32';
	export let scope: 'col' | 'colgroup' | 'row' | 'rowgroup' | null = 'col';

	export let choices: CheckBoxChoice[];
	export let group: string[];
	export let label: string;
	export let name: string;
	export let tooltip: string | undefined = undefined;
</script>

<TableColHead {widthClass} {scope}>
	<span class="inline-block">
		<FilterButton {choices} bind:group {label} {name} colorClass={tableHeaderColor} />
	</span>
	{#if tooltip}
		<Tooltip type="auto">{tooltip}</Tooltip>
	{/if}
</TableColHead>
