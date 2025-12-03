<script lang="ts">
	import { Button, Dropdown, Checkbox } from 'flowbite-svelte';
	import { FilterSolid, FilterOutline } from 'flowbite-svelte-icons';
	import { type CheckBoxChoice } from '$lib/core';

	export let choices: CheckBoxChoice[];
	export let group: string[];
	export let label: string;
	export let name: string;
	export let colorClass: string;
</script>

<div class="flex items-center justify-center">
	<Button
		color="alternative"
		class="w-fit border-none px-4 py-2 whitespace-nowrap shadow-none {colorClass}"
		style="border: none; box-shadow: none;"
	>
		{@html label}
		{#if group.length > 0}
			<FilterSolid class="ml-2 h-4 w-4" />
		{:else}
			<FilterOutline class="ml-2 h-4 w-4" />
		{/if}
	</Button>
	<Dropdown simple class="w-48 space-y-2 p-3 text-sm" style="text-transform: none;">
		<div class="flex flex-col gap-2">
			<Checkbox {name} {choices} bind:group />
		</div>
	</Dropdown>
</div>
