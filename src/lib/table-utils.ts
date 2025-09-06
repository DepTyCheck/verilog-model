import type { FoundError, CheckBoxChoice, MaintainersResponse } from '$lib/core';
import type { DisplayInfo } from '$lib/index';
import { displayIssueNovelty } from '$lib/index';

export function createChoices<T>(
	errors: FoundError[],
	getValue: (error: FoundError) => T,
	getLabel: (value: T) => string
): CheckBoxChoice[] {
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

export function applyFilter<T>(
	errors: FoundError[],
	selectedValues: string[],
	getValue: (error: FoundError) => T
): FoundError[] {
	if (selectedValues.length === 0) return errors;

	return errors.filter((error) => {
		const value = getValue(error);
		return value !== null && value !== undefined && selectedValues.includes(String(value));
	});
}

export function applyFilters(
	errors: FoundError[],
	filters: Array<{
		selectedValues: string[];
		getValue: (error: FoundError) => string | null | undefined;
	}>
): FoundError[] {
	return filters.reduce((filteredErrors, filter) => {
		return applyFilter(filteredErrors, filter.selectedValues, filter.getValue);
	}, errors);
}

export function createToolFilter(selectedValues: string[]) {
	return {
		selectedValues,
		getValue: (error: FoundError) => error.tool
	};
}

export function createNoveltyFilter(selectedValues: string[]) {
	return {
		selectedValues,
		getValue: (error: FoundError) => error.issue_novelty
	};
}

export function createMaintainersFilter(selectedValues: string[]) {
	return {
		selectedValues,
		getValue: (error: FoundError) => error.maintainers_response
	};
}

export function createStageFilter(selectedValues: string[]) {
	return {
		selectedValues,
		getValue: (error: FoundError) => error.stage
	};
}
