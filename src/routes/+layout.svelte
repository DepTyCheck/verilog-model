<script lang="ts">
	import '../app.css';
	import Header from '$lib/components/Header.svelte';

	let { children } = $props();
</script>

<head>
	<title>Verilog model</title>
</head>

<Header />

<div class="pt-16">
	{@render children()}
</div>
