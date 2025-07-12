<script lang="ts">
  import { page } from '$app/state';
  import { allFoundErrors } from '$lib/generated_errors_data';
  import type { FoundError } from '$lib/core';
  import VCard from '$lib/components/error/VCard.svelte';
  import { defaultMargin } from '$lib/styles';
  import CodeBlock from '$lib/components/error/CodeBlock.svelte';
  import { formatDateDMY } from '$lib/index';
  import FieldDisplay from '$lib/components/error/FieldDisplay.svelte';

  let id: string = page.params.id;
  let foundError: FoundError | null = null;
  let errorMsg: string | null = '';

  foundError = allFoundErrors.find(e => e.id === id) ?? null;
  if (!foundError) {
    errorMsg = `No foundError found for id: ${id}`;
  }
</script>

<div class={defaultMargin}>
  {#if errorMsg}
    <div class="bg-red-100 text-red-800 p-4 rounded mb-4">{errorMsg}</div>
  {:else if foundError}
    <VCard heading={foundError.title} info="">
      {#if foundError.tool}
        <FieldDisplay label="Tool" value={foundError.tool} />
      {/if}
      {#if foundError.stage}
        <FieldDisplay label="Stage" value={foundError.stage} />
      {/if}
      {#if foundError.short_desc}
        <FieldDisplay label="Short desc" value={foundError.short_desc} />
      {/if}
      {#if foundError.issue_link}
        <FieldDisplay label="Issue link" value={foundError.issue_link} linkUrl={foundError.issue_link} />
      {/if}
    </VCard>
    <h2 class="text-lg font-semibold mt-6 mb-2">Examples:</h2>
    {#each foundError.examples as example, i}
    <div class="mb-6">
      <VCard heading="" info="">
        {#if example.first_found}
          <div class="mb-2">
            <div class="text-xs text-gray-500">First found</div>
            <div class="text-lg font-medium">{formatDateDMY(new Date(example.first_found))}</div>
          </div>
        {/if}
        {#if example.minified_error != null && example.minified_error !== ''}
          <h3 class="font-semibold mt-2 mb-1">Minified example error</h3>
          <CodeBlock code={example.minified_error} />
        {/if}
        {#if example.minified_example != null && example.minified_example !== ''}
          <h3 class="font-semibold mt-4 mb-1">Minified example</h3>
          <CodeBlock code={example.minified_example} />
        {/if}
        {#if example.full_error != null && example.full_error !== ''}
          <h3 class="font-semibold mt-4 mb-1">Full example error</h3>
          <CodeBlock code={example.full_error} />
        {/if}
        {#if example.full_example != null && example.full_example !== ''}
          <h3 class="font-semibold mt-4 mb-1">Full example</h3>
          <CodeBlock code={example.full_example} />
        {/if}
      </VCard>
    </div>
    {/each}
  {/if}
</div>