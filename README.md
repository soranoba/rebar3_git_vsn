rebar3_git_vsn
======
[![CircleCI](https://circleci.com/gh/soranoba/rebar3_git_vsn/tree/master.svg?style=svg)](https://circleci.com/gh/soranoba/rebar3_git_vsn/tree/master)
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_git_vsn.svg)](https://hex.pm/packages/rebar3_git_vsn)

Provider for [rebar3](https://github.com/erlang/rebar3) to generate application versions from git.

## Overview
When using `{vsn, git}` in the application files, it will be generated automatically from the git hash.
This is not recommended when publishing to [hex](https://hex.pm).

If your library include escripts and does not use `{vsn, git}`, this plugin would be useful in order to return the correct version.  
For example, it is used in [erlup](https://github.com/soranoba/erlup).

The plugin works as follows:

- If `${APP_DIR}/.git` exists, add the git-derived version to the `.app` file
- If `${APP_DIR}/.git` does not exist, it does nothing

## Usage

```erlang
%% rebar.config
{plugins, [rebar3_git_vsn]}.

{provider_hooks, [{post, [{compile, git_vsn}]}]}.

{git_vsn,
 [
  %% Where to write the git vsn.
  %% FYI: application:get_env(Application, EnvKey)
  %%
  %% (default: git_vsn)
  {env_key, git_vsn},

  %% Git describe option.
  %% FYI: https://git-scm.com/docs/git-describe
  %%
  %% (default: "")
  {describe_opt, "--tags --long"},

  %% If the "git describe" returns "tag-count-hash", it convert to "{tag, count, hash}".
  %% Otherwise, it doesn't perform the conversion.
  %%
  %% (default: false)
  {separate, true}
 ]}.
```

## License
[MIT License](LICENSE)
