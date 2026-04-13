<script lang="ts">
	import { Badge } from 'flowbite-svelte';
	import type { IssueType } from '$lib/core';
	import { getIssueTypeDisplay } from '$lib/index';

	interface Props {
		types: IssueType[] | null;
	}

	let { types }: Props = $props();
</script>

{#if types && types.length > 0}
	<div class="flex flex-wrap gap-x-2 gap-y-1">
		{#each types as itype}
			{@const display = getIssueTypeDisplay(itype)}
			<Badge color={display.color} rounded>{display.text}</Badge>
		{/each}
	</div>
{/if}
