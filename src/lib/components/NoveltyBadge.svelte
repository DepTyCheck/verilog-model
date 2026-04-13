<script lang="ts">
	import { Badge, Tooltip } from 'flowbite-svelte';
	import type { IssueNovelty } from '$lib/core';
	import { displayIssueNovelty, getNoveltyTooltip } from '$lib/index';

	interface Props {
		novelty: IssueNovelty;
	}

	let { novelty }: Props = $props();
	const display = $derived(displayIssueNovelty(novelty));
</script>

{#if display.text}
	<Badge color={display.color} large>{display.text}</Badge>
	<Tooltip type="auto">{getNoveltyTooltip(novelty)}</Tooltip>
{/if}
