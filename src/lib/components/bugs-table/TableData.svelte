<script lang="ts">
	export let specialClass: string = '';
	export let widthClass: string = 'w-32';
	export let textAlign: string = 'text-center';
</script>

<td class="h-24 {widthClass} px-4 py-3 {textAlign} {specialClass}">
	<slot />
</td>
