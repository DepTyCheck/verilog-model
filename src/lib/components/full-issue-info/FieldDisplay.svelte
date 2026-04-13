<script lang="ts">
	import type { Snippet } from 'svelte';
	import { A } from 'flowbite-svelte';

	interface Props {
		label?: string;
		value?: string;
		linkUrl?: string;
		children?: Snippet;
	}

	let { label = '', value = '', linkUrl = '', children }: Props = $props();
</script>

<div class="flex items-start gap-6 py-2.5">
	<span class="w-36 shrink-0 pt-0.5 text-xs uppercase tracking-wider text-gray-400 dark:text-gray-500">{label}</span>
	<span class="text-sm text-gray-900 dark:text-gray-100">
		{#if children}
			{@render children()}
		{:else if linkUrl}
			<A href={linkUrl} class="underline" target="_blank" rel="noopener">{value}</A>
		{:else}
			{value}
		{/if}
	</span>
</div>
