<script lang="ts">
	import type { IssueLink } from '$lib/core';
	import { fixLink } from '$lib';

	interface Props {
		link: IssueLink;
		class?: string;
	}

	let { link, class: className = '' }: Props = $props();

	function extractIssueNumber(url: string): string | null {
		const match = url.match(/\/issues\/(\d+)/);
		return match ? `#${match[1]}` : null;
	}

	function getDisplayText(link: IssueLink): string {
		if (link.link_name) {
			return link.link_name;
		}

		if (link.local_id) {
			return link.local_id;
		}

		if (link.url) {
			const issueNumber = extractIssueNumber(link.url);
			return issueNumber || 'Link';
		}

		return 'Link';
	}

	function getIconPath(link: IssueLink): string | null {
		if (!link.link_icon) return null;
		return fixLink(`/icons/${link.link_icon}`);
	}
</script>

{#snippet linkContent()}
	{#if getIconPath(link)}
		<img src={getIconPath(link)} alt="" width="16" height="16" class="issue-link-icon" />
	{/if}
	{getDisplayText(link)}
{/snippet}

{#if link.url}
	<a href={link.url} target="_blank" rel="noopener noreferrer" class={className}>
		{@render linkContent()}
	</a>
{:else if link.local_id}
	<a href={fixLink(`/error/${link.local_id}`)} class={className}>
		{@render linkContent()}
	</a>
{/if}

<style>
	a {
		display: inline-flex;
		align-items: center;
		gap: 4px;
	}

	.issue-link-icon {
		width: 16px;
		height: 16px;
		flex-shrink: 0;
	}
</style>
