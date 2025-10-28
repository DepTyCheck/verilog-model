<script lang="ts">
	export let heading: string = '';
	export let info: string = '';

	import { Card } from 'flowbite-svelte';
</script>

<Card size="xl" class="max-w-none p-4 shadow-sm sm:p-6">
	{#if heading}
		<h3 class="mb-2 text-xl font-semibold dark:text-white">{heading}</h3>
	{/if}
	{#if info}
		<p class="mb-4 text-base font-normal text-gray-500 dark:text-gray-300">{info}</p>
	{/if}
	<slot />
</Card>
