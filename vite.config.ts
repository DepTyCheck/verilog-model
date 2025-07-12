import tailwindcss from '@tailwindcss/vite';
import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, searchForWorkspaceRoot } from 'vite';

export default defineConfig({
	plugins: [tailwindcss(), sveltekit()],
	server: {
		fs: {
		  allow: [
			// search up for workspace root
			searchForWorkspaceRoot(process.cwd()),
			// your custom rules
		  ],
		},
	  },
});
