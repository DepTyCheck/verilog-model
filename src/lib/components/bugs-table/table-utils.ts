import type { FoundError, CheckBoxChoice, IssueType, MaintainersResponse } from '$lib/core';
import type { DisplayInfo } from '$lib/index';
import { displayIssueNovelty, getIssueTypeDisplay } from '$lib/index';

export function createChoices<T>(errors: FoundError[], getValue: (error: FoundError) => T, getLabel: (value: T) => string): CheckBoxChoice[] {
	return Array.from(new Set(errors.map(getValue)))
		.filter((value) => value !== null && value !== undefined)
		.map((value) => ({
			value: String(value),
			label: getLabel(value)
		}))
		.sort((a, b) => a.label.localeCompare(b.label));
}

export function createToolChoices(errors: FoundError[]): CheckBoxChoice[] {
	return createChoices(
		errors,
		(error) => error.tool,
		(tool) => tool
	);
}

export function createStageChoices(errors: FoundError[]): CheckBoxChoice[] {
	return createChoices(
		errors,
		(error) => error.stage,
		(stage) => stage!
	);
}

export function createNoveltyChoices(errors: FoundError[]): CheckBoxChoice[] {
	return createChoices(
		errors,
		(error) => error.issue_novelty,
		(novelty) => displayIssueNovelty(novelty!).text
	);
}

export function createMaintainersChoices(
	errors: FoundError[],
	getMaintainersResponseDisplay: (response: MaintainersResponse) => DisplayInfo
): CheckBoxChoice[] {
	return createChoices(
		errors,
		(error) => error.maintainers_response,
		(response) => getMaintainersResponseDisplay(response!).text
	);
}

type FilterValue = string | null | undefined;

export function applyFilter(
	errors: FoundError[],
	selectedValues: string[],
	getValue: (error: FoundError) => FilterValue | FilterValue[]
): FoundError[] {
	if (selectedValues.length === 0) return errors;

	return errors.filter((error) => {
		const value = getValue(error);
		if (value === null || value === undefined) return false;
		if (Array.isArray(value)) {
			return value.some((v) => v !== null && v !== undefined && selectedValues.includes(String(v)));
		}
		return selectedValues.includes(String(value));
	});
}

export function applyFilters(
	errors: FoundError[],
	filters: Array<{
		selectedValues: string[];
		getValue: (error: FoundError) => FilterValue | FilterValue[];
	}>
): FoundError[] {
	return filters.reduce((filteredErrors, filter) => {
		return applyFilter(filteredErrors, filter.selectedValues, filter.getValue);
	}, errors);
}

export function createFilter(selectedValues: string[], getValue: (error: FoundError) => FilterValue | FilterValue[]) {
	return { selectedValues, getValue };
}

export function createIssueTypeChoices(errors: FoundError[]): CheckBoxChoice[] {
	const allTypes = new Set<IssueType>();
	for (const error of errors) {
		if (error.issue_type) {
			for (const t of error.issue_type) {
				allTypes.add(t);
			}
		}
	}
	return Array.from(allTypes)
		.filter((t): t is NonNullable<IssueType> => t !== null)
		.map((t) => ({ value: t, label: getIssueTypeDisplay(t).text }))
		.sort((a, b) => a.label.localeCompare(b.label));
}
