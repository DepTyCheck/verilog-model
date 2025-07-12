import type { EntryGenerator } from './$types';
import { allFoundErrors } from '$lib/generated/errors_data';

// https://svelte.dev/docs/kit/page-options#entries
export const entries: EntryGenerator = () => {
	return allFoundErrors.map((error) => ({ id: error.id }));
};

export const prerender = true;
