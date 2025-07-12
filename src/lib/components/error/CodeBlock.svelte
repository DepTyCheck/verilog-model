<script lang="ts">
  export let code: string = '';
  let copied = false;

  async function copyToClipboard() {
    await navigator.clipboard.writeText(code);
    copied = true;
    setTimeout(() => copied = false, 1200);
  }
  import { Button } from 'flowbite-svelte';
</script>

<div class="relative bg-gray-900 text-gray-100 rounded-lg p-4 font-mono overflow-x-auto">
  <Button
    class="absolute top-2 right-2 bg-gray-700 hover:bg-gray-600 text-xs text-white px-2 py-1 rounded focus:outline-none focus:ring"
    onclick={copyToClipboard}
    aria-label="Copy code"
    size="xs"
    color="gray"
    >
    {#if copied}
      Copied!
    {:else}
      Copy
    {/if}
  </Button>
  <pre class="whitespace-pre-wrap break-words"><code>{code}</code></pre>
</div>
