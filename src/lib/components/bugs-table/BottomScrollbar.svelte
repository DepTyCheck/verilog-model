<script lang="ts">
	import { onMount, onDestroy } from 'svelte';

	export let scrollContainer: HTMLElement | undefined;

	let bottomScrollbar: HTMLInputElement;
	let showBottomScrollbar = false;

	function updateVisibility() {
		if (!scrollContainer) return;
		showBottomScrollbar = scrollContainer.scrollWidth > scrollContainer.clientWidth;
	}

	function handleContainerScroll() {
		if (!scrollContainer || !bottomScrollbar) return;
		updateVisibility();

		const maxScroll = scrollContainer.scrollWidth - scrollContainer.clientWidth;
		const scrollPercentage = maxScroll > 0 ? (scrollContainer.scrollLeft / maxScroll) * 100 : 0;
		bottomScrollbar.value = scrollPercentage.toString();
	}

	function handleRangeInput() {
		if (!scrollContainer || !bottomScrollbar) return;
		const maxScroll = scrollContainer.scrollWidth - scrollContainer.clientWidth;
		const scrollLeft = (parseFloat(bottomScrollbar.value) / 100) * maxScroll;
		scrollContainer.scrollLeft = scrollLeft;
	}

	$: if (scrollContainer) {
		// Initial check
		updateVisibility();

		scrollContainer.addEventListener('scroll', handleContainerScroll, { passive: true });
	}

	// if scrollContainer changes, we should remove the old listener.
	let oldContainer: HTMLElement | undefined;
	$: if (scrollContainer !== oldContainer) {
		if (oldContainer) {
			oldContainer.removeEventListener('scroll', handleContainerScroll);
		}
		if (scrollContainer) {
			scrollContainer.addEventListener('scroll', handleContainerScroll, { passive: true });
			updateVisibility();
		}
		oldContainer = scrollContainer;
	}

	onMount(() => {
		window.addEventListener('resize', updateVisibility);
		// Initial check in case scrollContainer is already bound
		updateVisibility();
	});

	onDestroy(() => {
		if (typeof window !== 'undefined') {
			window.removeEventListener('resize', updateVisibility);
		}
		if (scrollContainer) {
			scrollContainer.removeEventListener('scroll', handleContainerScroll);
		}
	});
</script>

{#if showBottomScrollbar}
	<div
		class="fixed right-8 bottom-4 left-8 z-50 rounded-xl border border-gray-200 bg-white/90 shadow-xl backdrop-blur-sm dark:border-gray-700 dark:bg-gray-900/90"
	>
		<div class="flex justify-center px-4 py-3">
			<div class="w-full max-w-4xl">
				<input
					bind:this={bottomScrollbar}
					type="range"
					min="0"
					max="100"
					value="0"
					on:input={handleRangeInput}
					class="h-3 w-full cursor-pointer appearance-none rounded-lg bg-gray-200 dark:bg-gray-700"
				/>
			</div>
		</div>
	</div>
{/if}
