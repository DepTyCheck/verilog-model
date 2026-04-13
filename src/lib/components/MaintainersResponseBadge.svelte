<script lang="ts">
	import { Badge, Tooltip } from 'flowbite-svelte';
	import type { MaintainersResponse } from '$lib/core';
	import { getMaintainersResponseDisplay, getMaintainersResponseTooltip } from '$lib/index';

	interface Props {
		response: MaintainersResponse;
	}

	let { response }: Props = $props();
	const display = $derived(getMaintainersResponseDisplay(response));
</script>

<Badge color={display.color} large rounded>{display.text}</Badge>
<Tooltip type="auto">{getMaintainersResponseTooltip(response)}</Tooltip>
