#!/bin/bash

DIR=$(dirname $0)
REBAR=$(pwd)/${DIR}/../rebar3
SHOW_ENV=$(pwd)/${DIR}/show_env.escript
APP_PATH=_build/default/lib/spam/ebin/spam.app

pushd $(pwd)

test_cases=(
    'spam_1 [{git,"eb4236d"}]'
    'spam_2 [{git_vsn,"f4ce5f0"}]'
    'spam_3 [{git,{"0.0.1","1","g7771601"}}]'
    'spam_4 [{git,{"0.0.2","0","g7771601"}}]'
)

for test_case in "${test_cases[@]}"; do
  test_case=(${test_case[@]})

  pushd ${DIR}/../sample/${test_case[0]}
  ${REBAR} compile
  result=$(escript ${SHOW_ENV} ${APP_PATH})

  if [ "${test_case[1]}" != ${result} ]; then
      >&2 echo "FAILED: ${test_case[0]} expected ${test_case[1]}, got ${result}"
      exit 1
  fi
  popd
done

popd
