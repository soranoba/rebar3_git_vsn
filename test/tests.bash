#!/bin/bash -e

DIR=$(dirname $0)
REBAR=$(which rebar3)
SHOW_ENV=$(pwd)/${DIR}/show_env.escript
APP_PATH=_build/default/lib/spam/ebin/spam.app

pushd $(pwd)

test_cases=(
    'spam_1 [{git,"VSN"}]'
    'spam_2 [{git_vsn,"VSN"}]'
    'spam_3 [{git,{"1.0.0","1","gVSN"}}]'
    'spam_4 [{git,{"1.0.0","0","gVSN"}}]'
    'spam_5 "1.0.0" vsn'
    'spam_6 "1.0.0-1-VSN" vsn'
    'spam_7 "1.0.0-1-gVSN" vsn'
)

for test_case in "${test_cases[@]}"; do
  test_case=(${test_case[@]})

  pushd ${DIR}/../sample/${test_case[0]}

  rm -fr .git
  git init >/dev/null
  git add .gitignore >/dev/null
  git commit -am "Test" >/dev/null
  if [ ${test_case[0]} != 'spam_1' -a ${test_case[0]} != 'spam_2' ]; then
    git tag 1.0.0 >/dev/null
  fi
  if [ ${test_case[0]} != 'spam_4' -a ${test_case[0]} != 'spam_5' ]; then
    git add rebar.config >/dev/null
    git commit -am "Test" >/dev/null
  fi

  VSN=$(git describe --abbrev=7 --always --long)

  EXPECT=${test_case[1]//VSN/$VSN}

  ${REBAR} compile
  result=$(escript ${SHOW_ENV} ${APP_PATH} ${test_case[2]})

  if [ "${EXPECT}" != ${result} ]; then
      >&2 echo "FAILED: ${test_case[0]} expected ${EXPECT}, got ${result}"
      exit 1
  fi
  popd
done

popd
