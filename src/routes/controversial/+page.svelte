<script lang="ts">
	import BugsTable from '$lib/components/bugs-table/BugsTable.svelte';
	import { defaultMargin } from '$lib/styles';
	import { A } from 'flowbite-svelte';
	import { allFoundErrors } from '$lib/generated/errors-data';
	import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';

	const controversial = allFoundErrors.filter((e) => e.list === 'controversial');
</script>

<div class={defaultMargin}>
	<BugsTable errors={controversial} heading="Controversial issues" {description} />
</div>

{#snippet description()}
	<p class="text-base font-normal text-gray-500 dark:text-gray-300">
		This list gathers controversial HDL behaviors discovered by
		<A href={githubUrl} class="underline">verilog-model</A> using
		<A href={depTyCheckGithubUrl} class="underline">DepTyCheck</A>.
		<br />
		It highlights cases where the IEEE standard is unclear, leading to different interpretations among
		HDL analysis tools that may or may not be considered errors.
	</p>
{/snippet}
