<script lang="ts">
	import type { IssueLink } from '$lib/core';
	import IssueLinkAnchor from '$lib/components/IssueLinkAnchor.svelte';

	interface Props {
		issueLinks?: IssueLink[];
	}

	let { issueLinks = [] }: Props = $props();
</script>

{#if issueLinks && issueLinks.length > 0}
	<div class="flex flex-col gap-1">
		{#each issueLinks as link}
			<div>
				<IssueLinkAnchor {link} class="text-sm text-primary-600 underline hover:text-primary-800" />
			</div>
		{/each}
	</div>
{/if}
