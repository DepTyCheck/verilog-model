<script lang="ts">
	import { GithubSolid } from 'flowbite-svelte-icons';
	import { Navbar, NavBrand, A } from 'flowbite-svelte';
	import { githubUrl } from '$lib/consts';
	import { LinkHandler } from '$lib';
</script>

<header>
	<Navbar
		class="fixed top-0 z-40 w-full border-b border-gray-200 bg-white dark:border-gray-600 dark:bg-gray-800"
	>
		<NavBrand href={LinkHandler('/')}>
			<img src={LinkHandler('/logo_no_bg.png')} class="mr-2 h-8 sm:h-9" alt="Project Logo" />
			<span class="self-center text-xl font-semibold whitespace-nowrap dark:text-white"
				>Verilog model</span
			>
		</NavBrand>
		<A href={githubUrl}>
			<GithubSolid />
		</A>
	</Navbar>
</header>
