import { type FoundError } from '$lib/core';
import { resolve } from '$app/paths';
import type { IssueNovelty, MaintainersResponse } from './core';

export interface DisplayInfo {
	text: string;
	color: 'red' | 'blue' | 'purple' | 'gray' | 'green' | 'yellow' | 'indigo' | 'pink';
}

export function getFirstFound(error: FoundError): Date | null {
	const dates: Date[] = error.examples
		.map((v) => {
			if (!v.first_found) return null;
			// first_found is string
			const d = new Date(v.first_found);
			return isNaN(d.getTime()) ? null : d;
		})
		.filter((d): d is Date => d !== null);

	if (dates.length === 0) return null;

	return dates.reduce((earliest, date) => (date < earliest ? date : earliest), dates[0]);
}

export function formatDateDMY(date: Date | null): string {
	if (!date || isNaN(date.getTime())) return '';
	const dd = String(date.getDate()).padStart(2, '0');
	const mm = String(date.getMonth() + 1).padStart(2, '0');
	const yyyy = date.getFullYear();
	return `${dd}.${mm}.${yyyy}`;
}

export function displayIssueNovelty(status: IssueNovelty): DisplayInfo {
	switch (status) {
		case 'new':
			return { text: 'Brand new', color: 'pink' };
		case 'already_known':
			return { text: 'Known before', color: 'indigo' };
		case 'unsupported':
			return { text: 'Unsupported', color: 'green' };
		case 'feature':
			return { text: 'Feature', color: 'gray' };
		case 'late':
			return { text: 'Late', color: 'gray' };
		default:
			return { text: '', color: 'gray' };
	}
}

export function getMaintainersResponseDisplay(tag: MaintainersResponse): DisplayInfo {
	switch (tag) {
		case 'bug':
			return { text: 'Bug', color: 'red' };
		case 'enhancement':
			return { text: 'Enhancement', color: 'blue' };
		case 'low':
			return { text: 'Low priority', color: 'purple' };
		case 'wontfix':
			return { text: "Won't fix", color: 'gray' };
		default:
			return { text: 'No response', color: 'green' };
	}
}

export function getMaintainersResponseTooltip(status: MaintainersResponse): string {
	switch (status) {
		case 'bug':
			return 'The maintainers have accepted this bug';
		case 'enhancement':
			return "This feature isn't implemented yet, but it's planned";
		case 'low':
			return 'The maintainers are unsure whether this feature is needed';
		case 'wontfix':
			return 'There are no plans to implement this feature';
		case null:
			return 'No response information available';
		default:
			return status;
	}
}

export function getNoveltyTooltip(status: IssueNovelty): string {
	switch (status) {
		case 'new':
			return 'This issue is reported for the first time';
		case 'already_known':
			return 'This issue was already known before';
		case 'unsupported':
			return 'This feature is not supported by the tool and is not planned in the near future';
		case 'feature':
			return 'Actually it is not a bug, but a feature';
		case 'late':
			return 'An issue was detected, but the maintainers resolved it before it was reported';
		case null:
			return 'No novelty information available';
		default:
			return `Novelty: ${status}`;
	}
}

export const fixLink = (link: string | undefined) => {
	if (!link) return '';
	return resolve(link as any);
};
