export type CheckBoxChoice = {
	value: string;
	label: string;
};

export type IssueStatus = 'reported' | 'already_known' | 'unsupported' | null;

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
	issue_status?: IssueStatus;
};

export type SortableColumn = 'tool' | 'firstFound' | 'title' | 'stage' | 'issue_link' | 'issue_status';
