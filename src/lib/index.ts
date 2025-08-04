import { type FoundError } from '$lib/core';
import { base } from '$app/paths'
import type { IssueStatus } from './core';

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

export function displayIssueStatus(status: IssueStatus | undefined): string {
  switch (status) {
    case 'reported': return 'Brand new';
    case 'already_known': return 'Known before';
	  case 'unsupported': return 'Unsupported';
    case undefined:
      return '';
    default:
      return '';
  }
}

export const LinkHandler = (link: string) => {
  if (process.env.NODE_ENV === 'development') {
    return link
  }

  if (link === '/') {
    return base
  }

  return base + link
}
