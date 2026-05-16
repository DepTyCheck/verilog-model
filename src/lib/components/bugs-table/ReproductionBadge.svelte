<script lang="ts">
	import { CheckCircleSolid, CloseCircleSolid, CircleMinusOutline } from 'flowbite-svelte-icons';
	import { Tooltip } from 'flowbite-svelte';
	import type { ReproducedState } from '$lib/generated/errors-regression';
	import { reproducedDisplay } from '$lib/components/bugs-table/regression-utils';

	interface Props {
		state: ReproducedState;
		showText?: boolean;
	}

	let { state, showText = false }: Props = $props();
	const display = $derived(reproducedDisplay[state]);
</script>

<span class="inline-flex items-center gap-1">
	{#if state === 'reproduced'}
		<CheckCircleSolid class="text-green-500" />
	{:else if state === 'not_reproduced'}
		<CloseCircleSolid class="text-red-500" />
	{:else}
		<CircleMinusOutline class="text-gray-400" />
	{/if}
	{#if showText}
		<span class="text-sm">{display.label}</span>
	{/if}
</span>
<Tooltip type="auto">{display.tooltip}</Tooltip>
