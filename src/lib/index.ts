import { type FoundError } from '$lib/core';

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

export function displayIssueStatus(status: string | null | undefined): string {
  switch (status) {
    case 'reported': return 'New';
    case 'already_known': return 'Known';
    case 'wont_report': return 'Not planned';
	case 'unsupported': return 'Unsupported';
    case null:
    case undefined:
    case '':
      return '';
    default:
      return status;
  }
}
