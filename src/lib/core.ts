export type CheckBoxChoice = {
	value: string;
	label: string;
};

export type IssueNovelty = 'new' | 'already_known' | 'unsupported' | 'late' | null;
export type MaintainersResponse = 'bug' | 'enhancement' | 'low' | 'wontfix' | null;
export type IssueType = 'crash' | 'bad_message' | 'infinite_loop' | 'feature' | null;
export type IssueList = 'issues' | 'controversial';

export type IssueLink = {
	url?: string;
	local_id?: string;
	link_name?: string;
	link_icon?: string;
};

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
	profile: string;
	target: string;
	id: string;
	title: string;
	short_desc?: string | null;
	regex?: string | null;
	examples: FoundErrorExample[];
	issue_type: IssueType[] | null;
	issue_links?: IssueLink[];
	issue_novelty: IssueNovelty;
	maintainers_response: MaintainersResponse;
	list: IssueList;
};

export type SortableColumn = 'firstFound' | 'title' | 'stats';

export type SortDirection = 'asc' | 'desc';
