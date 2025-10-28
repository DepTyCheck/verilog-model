<script lang="ts">
	export let label: string = '';
	export let value: string = '';
	export let linkUrl: string = '';

	import { A } from 'flowbite-svelte';
</script>

<div class="mb-2">
	<div class="text-xs text-gray-500">{label}</div>
	<div class="text-lg font-medium">
		{#if linkUrl}
			<A href={linkUrl} class="underline" target="_blank" rel="noopener">{value}</A>
		{:else}
			{value}
		{/if}
	</div>
</div>
