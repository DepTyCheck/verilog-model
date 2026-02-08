<script lang="ts">
	import { A } from 'flowbite-svelte';
	import type { IssueLink } from '$lib/core';

	interface Props {
		issueLinks?: IssueLink[];
	}

	let { issueLinks = [] }: Props = $props();

	function extractIssueNumber(url: string): string | null {
		const match = url.match(/\/issues\/(\d+)/);
		return match ? `#${match[1]}` : null;
	}

	function getDisplayText(link: IssueLink): string {
		// Use link_name if available, otherwise extract from URL
		if (link.link_name) {
			return link.link_name;
		}
		
		const issueNumber = extractIssueNumber(link.url);
		return issueNumber || 'Link';
	}
</script>

{#if issueLinks && issueLinks.length > 0}
	<div class="flex flex-col gap-1">
		{#each issueLinks as link}
			<div>
				<A href={link.url} target="_blank" rel="noopener noreferrer" class="text-sm">
					{getDisplayText(link)}
				</A>
			</div>
		{/each}
	</div>
{/if}
