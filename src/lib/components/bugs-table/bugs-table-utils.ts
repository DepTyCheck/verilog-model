import { getFirstFound } from '$lib';
import type { FoundError, SortableColumn, SortDirection } from '$lib/core';
import type { ErrorPercentages } from './error-stats-utils';

function compare<T>(a: T, b: T): number {
	if (a < b) return -1;
	if (a > b) return 1;
	return 0;
}

export class SortedIssues {
	constructor(
		private errors: FoundError[],
		private errorPercentages: Record<string, ErrorPercentages>
	) {}

	private sortByTitle(a: FoundError, b: FoundError): number {
		return compare(a.title, b.title);
	}

	private sortByFirstFound(a: FoundError, b: FoundError): number {
		const aVal = getFirstFound(a)?.getTime() ?? 0;
		const bVal = getFirstFound(b)?.getTime() ?? 0;

		return compare(aVal, bVal);
	}

	private sortByStats(a: FoundError, b: FoundError): number {
		const aVal = this.errorPercentages[a.id]?.overall ?? 0;
		const bVal = this.errorPercentages[b.id]?.overall ?? 0;

		return compare(aVal, bVal);
	}

	sorted(sortColumn: SortableColumn, sortDest: SortDirection): FoundError[] {
		let comparator: (a: FoundError, b: FoundError) => number;

		switch (sortColumn) {
			case 'title':
				comparator = (a, b) => this.sortByTitle(a, b);
				break;
			case 'firstFound':
				comparator = (a, b) => this.sortByFirstFound(a, b);
				break;
			case 'stats':
				comparator = (a, b) => this.sortByStats(a, b);
				break;
		}

		return [...this.errors].sort((a, b) => {
			const result = comparator(a, b);
			return sortDest === 'asc' ? result : -result;
		});
	}
}
