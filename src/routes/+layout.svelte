<script lang="ts">
	import '../app.css';
	import Header from '$lib/Header.svelte';
	import { base } from '$app/paths';

	let { children } = $props();
</script>

<head>
	<title>Verilog model</title>
</head>

<Header />

<div class="pt-16">
	{@render children()}
</div>
