rebar3_git_vsn
======
[![Build Status](https://travis-ci.com/soranoba/rebar3_git_vsn.svg?branch=master)](https://travis-ci.com/soranoba/rebar3_git_vsn)
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_git_vsn.svg)](https://hex.pm/packages/rebar3_git_vsn)

Provider for generate the version from git.

It is a plugin for [rebar3](https://github.com/erlang/rebar3)

## Overview
When we use the `{vsn, git}`, it can be generated to automatically version from git hash.  
However, it does not mean able to use at any time.  
For example, if you want to publish to [hex](https://hex.pm), it is not a recommended way.

If your library include escripts and does not use `{vsn, git}`, this plugin would be useful in order to return the correct version.  
For example, it is used in [erlup](https://github.com/soranoba/erlup).

This plugin works as follows:

- If `${APP_DIR}/.git` is exist, add the version of git to the `.app` file.
- If `${APP_DIR}/.git` isn't exist, it does noting.

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
