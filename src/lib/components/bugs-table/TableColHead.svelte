<script lang="ts">
	import { tableHeaderText, tableHeaderColor } from './table-styles';
	export let label: string = '';
	export let colorClass: string = tableHeaderColor;
	export let widthClass: string = 'w-32';
	export let scope: 'col' | 'colgroup' | 'row' | 'rowgroup' | null = 'col';
</script>

<th {scope} class={`px-4 py-3 ${widthClass} ${tableHeaderText} ${colorClass}`} on:click on:keydown on:keyup on:keypress {...$$restProps}>
	{#if label}
		<span class="whitespace-nowrap">{@html label}</span>
	{/if}
	<slot />
</th>
