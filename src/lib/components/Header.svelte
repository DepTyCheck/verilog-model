<script lang="ts">
	import { GithubSolid } from 'flowbite-svelte-icons';
	import { Navbar, NavBrand, A, Button, ButtonGroup } from 'flowbite-svelte';
	import { githubUrl } from '$lib/consts';
	import { fixLink } from '$lib';
	import { page } from '$app/state';

	const path = $derived(page.url.pathname.replace(/\/$/, ''));
	const isWarnings = $derived(path.endsWith('/warnings'));
	const isControversial = $derived(path.endsWith('/controversial'));
	const isIssues = $derived(!isWarnings && !isControversial);
</script>

<header>
	<Navbar class="fixed top-0 z-40 w-full border-b border-gray-200 bg-white dark:border-gray-600 dark:bg-gray-800">
		<div class="flex items-center gap-3">
			<NavBrand href={fixLink('/')}>
				<img src={fixLink('/icons/logo_no_bg.png')} class="mr-2 h-8 sm:h-9" alt="Project Logo" />
				<span class="self-center text-xl font-semibold whitespace-nowrap dark:text-white">Verilog model</span>
			</NavBrand>
			<ButtonGroup size="sm">
				<Button href={fixLink('/warnings')} color={isWarnings ? 'primary' : 'alternative'}>Warnings</Button>
				<Button href={fixLink('/')} color={isIssues ? 'primary' : 'alternative'}>Bugs & issues</Button>
				<Button href={fixLink('/controversial')} color={isControversial ? 'primary' : 'alternative'}>Controversial</Button>
			</ButtonGroup>
		</div>
		<A href={githubUrl}>
			<GithubSolid />
		</A>
	</Navbar>
</header>
