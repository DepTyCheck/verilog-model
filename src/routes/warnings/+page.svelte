<script lang="ts">
	import BugsTable from '$lib/components/bugs-table/BugsTable.svelte';
	import { defaultMargin } from '$lib/styles';
	import { A } from 'flowbite-svelte';
	import { allFoundErrors } from '$lib/generated/errors-data';
	import { githubUrl, depTyCheckGithubUrl } from '$lib/consts';

	const warnings = allFoundErrors.filter((e) => e.list === 'warnings');
</script>

<div class={defaultMargin}>
	<BugsTable errors={warnings} heading="Warnings" {description} />
</div>

{#snippet description()}
	<p class="text-base font-normal text-gray-500 dark:text-gray-300">
		This is a list of warnings in HDL analyzers found by
		<A href={githubUrl} class="underline">verilog-model</A> using
		<A href={depTyCheckGithubUrl} class="underline">DepTyCheck</A>.
		<br />
		DepTyCheck specifications can define expected warnings for tool outputs, allowing an oracle to verify their presence. Although the verilog-model
		CI does not yet perform this comparison, some warnings are printed in HDL designs and some warnings are observed in tool outputs.
	</p>
{/snippet}
