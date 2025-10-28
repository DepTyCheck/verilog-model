<script lang="ts">
	import { codeToHtml } from '$lib/generated/shiki.bundle';
	import { Button } from 'flowbite-svelte';
	import { onMount } from 'svelte';

	export let code: string = '';
	export let language: string = '';

	let copied = false;
	let highlightedCode = '';

	async function copyToClipboard() {
		await navigator.clipboard.writeText(code);
		copied = true;
		setTimeout(() => (copied = false), 1200);
	}

	onMount(async () => {
		if (code && language) {
			try {
				highlightedCode = await codeToHtml(code, {
					lang: language,
					theme: 'github-dark-default'
				});
			} catch (error) {
				console.error('Error highlighting code:', error);
				highlightedCode = code;
			}
		}
	});
</script>

<div
	class="relative overflow-x-auto rounded-lg p-4 font-mono text-gray-100"
	style="background-color: #0d1117;"
>
	<Button
		class="absolute top-2 right-2 rounded bg-gray-700 px-2 py-1 text-xs text-white hover:bg-gray-600 focus:ring focus:outline-none"
		onclick={copyToClipboard}
		aria-label="Copy code"
		size="xs"
		color="gray"
	>
		{#if copied}
			Copied!
		{:else}
			Copy
		{/if}
	</Button>
	{#if highlightedCode}
		{@html highlightedCode}
	{:else}
		<pre class="break-words whitespace-pre-wrap"><code>{code}</code></pre>
	{/if}
</div>
