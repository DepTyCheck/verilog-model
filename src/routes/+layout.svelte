<script lang="ts">
	import '../app.css';
	import Header from '$lib/Header.svelte';
	import Footer from '$lib/Footer.svelte';

	let { children } = $props();
</script>

<Header />

<div class="pt-16">
	{@render children()}
</div>

<Footer />
