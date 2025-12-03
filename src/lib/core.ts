export type CheckBoxChoice = {
	value: string;
	label: string;
};

export type IssueNovelty = 'new' | 'already_known' | 'unsupported' | 'feature' | 'late' | null;
export type MaintainersResponse = 'bug' | 'enhancement' | 'low' | 'wontfix' | null;

export type FoundErrorExample = {
	id: string;
	first_found?: string | null;
	minified_example?: string | null;
	minified_error?: string | null;
	full_error?: string | null;
	full_example?: string | null;
};

export type FoundError = {
	stage?: string | null;
	tool: string;
	id: string;
	title: string;
	short_desc?: string | null;
	examples: FoundErrorExample[];
	issue_link?: string | null;
	issue_novelty: IssueNovelty;
	maintainers_response: MaintainersResponse;
};

export type SortableColumn = 'firstFound' | 'title' | 'stats';
