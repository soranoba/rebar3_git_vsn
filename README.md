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

- If `${APP_DIR}/.git` exists, add the git-derived version to the `.app` file in the `env` key
- If `${APP_DIR}/.git` does not exist, it does nothing

## Usage

```erlang
%% rebar.config
{plugins, [rebar3_git_vsn]}.

{provider_hooks, [{post, [{compile, git_vsn}]}]}.

{git_vsn,
 [
  %% `{env_key, EnvKey}` - specifies where to write the git vsn under the `env` key.
  %%
  %% EnvKey:
  %%  * ignore  - don't modify the `env` key.
  %%
  %% (default: git_vsn)
  {env_key, git_vsn},

  %% `{vsn_format, VsnType}` - controls modification of the `vsn` application key.
  %%
  %% VsnType:
  %%  * ignore  - no modification
  %%  * ggitver - the output of `git describe --always --tags --long`
  %%  * gitver  - same as above, but the `g` prefix is removed from the trailing
  %%              git commit hash
  %%
  %% (default: ignore)
  {vsn_format, gitver},

  %% `{separate, boolean()}` - If the "git describe" returns "tag-count-hash",
  %%                           it will convert to "{tag, count, hash}".
  %%                           Otherwise, it doesn't perform the conversion.
  %% (default: false)
  {separate, true},

  %% `{describe_opt, String}` - Append `String` to `git describe --always`.
  %%
  %% Default:
  %%  * `"--abbrev=7 --tags"`         - when vsn_format is `gitver`
  %%  * `"--abbrev=7 --tags --long"`  - otherwise
 ]}.
```

## License
[MIT License](LICENSE)
