import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import path from 'node:path';

const REPO_URL_FALLBACK = 'https://github.com/DepTyCheck/verilog-model.git';

function run(cmd, args, opts = {}) {
  const result = spawnSync(cmd, args, { stdio: 'inherit', ...opts });
  if (result.error) {
    console.error(`Failed to spawn '${cmd}': ${result.error.message}`);
    process.exit(1);
  }
  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
}

function repoUrl() {
  const r = spawnSync('git', ['config', '--get', 'remote.origin.url'], { encoding: 'utf8' });
  if (r.status === 0) {
    const url = r.stdout.trim();
    if (url) return url;
  }
  return REPO_URL_FALLBACK;
}

function step(label, condition, action) {
  if (condition()) {
    console.log(`✓ ${label} (skipped — already present)`);
    return;
  }
  console.log(`→ ${label}`);
  action();
}

function main() {
  const url = repoUrl();
  const cwd = process.cwd();

  step('npm install',
    () => existsSync(path.join(cwd, 'node_modules')),
    () => run('npm', ['install']));

  step('clone dataset branch',
    () => existsSync(path.join(cwd, 'dataset')),
    () => run('git', ['clone', '--branch', 'dataset', '--single-branch', url, 'dataset']));

  step('clone ci/save-dataset branch into ./verilog-model',
    () => existsSync(path.join(cwd, 'verilog-model')),
    () => run('git', ['clone', '--branch', 'ci/save-dataset', '--single-branch', url, 'verilog-model']));

  step('create python venv + install requirements',
    () => existsSync(path.join(cwd, 'verilog-model', '.venv')),
    () => {
      run('python3', ['-m', 'venv', '.venv'], { cwd: path.join(cwd, 'verilog-model') });
      run('./.venv/bin/pip', ['install', '-r', 'ci/runner/dataset_stats/requirements.txt'],
        { cwd: path.join(cwd, 'verilog-model') });
    });

  step('generate combined_stats.csv',
    () => existsSync(path.join(cwd, 'combined_stats.csv')),
    () => run('./.venv/bin/python', [
      '-m', 'dataset_stats.main',
      '--issues-csv', '../dataset/issues.csv',
      '--files-dir', '../dataset/files',
      '--found-issues-dir', '../dataset/found_issues',
      '--legacy-stats-csv', '../dataset/legacy_stats.csv',
      '--output', '../combined_stats.csv'
    ], {
      cwd: path.join(cwd, 'verilog-model'),
      env: { ...process.env, PYTHONPATH: 'ci/runner' }
    }));

  console.log('Bootstrap complete.');
}

main();
