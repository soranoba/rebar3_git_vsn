rebar3_git_vsn
======
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_git_vsn.svg)](https://hex.pm/packages/rebar3_git_vsn)

Provider for generate the version from git.

It is a plugin for [rebar3](https://github.com/erlang/rebar3)

## Overview
When we use the `{vsn, git}`, it can be generated to automatically version from git hash.  
However, it does not mean use at any time.  
For example, if you want to publish to [hex](https://hex.pm), it is not a recommended way.

This plugin works as follows:

- If `${APP_DIR}/.git` is exist, add the version of git to the `.app` file.
- If `${APP_DIR}/.git` isn't exist, it does noting.

This will help in providing functions such as `my_escript --version`.

## Usage

```erlang
%% rebar.config
{plugins, [rebar3_git_vsn]}.

{provider_hooks, [{post, [{compile, git_vsn}]}]}.

{git_vsn,
 [
  %% Where to write the git vsn. (default: git_vsn)
  %%
  %% application:get_env(Application, EnvKey)
  {env_key, git_vsn},

  %% Git describe option. https://git-scm.com/docs/git-describe (default: "")
  {describe_opt, "--tags --long"}
 ]}.
```

## License
[MIT License](LICENSE)
