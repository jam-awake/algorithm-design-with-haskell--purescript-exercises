#!/usr/bin/env bash

# ./mkexercise.sh p 1 2 -- chapter 1's 2nd exercise (PureScript file)
# ./mkexercise.sh m 1 2 -- chapter 1's 2nd exercise (Markdown file)

FILE_TYPE_ARG="$1"
CHAPTER_ARG="$2"
EXERCISE_ARG="$3"

case "${FILE_TYPE_ARG}" in
  "p")
    EXERCISE_EXT=".purs"
    ;;
  "m")
    EXERCISE_EXT=".md"
    ;;

  *)
    echo "Invalid argument for first arg; expected 'p' or 'm'"
    exit 1
    ;;
esac

CHAPTER_NUM=$(printf "%02d\n" "${CHAPTER_ARG}")
EXERCISE_NUM=$(printf "%02d\n" "${EXERCISE_ARG}")

CHAPTER_MODULE="Chapter${CHAPTER_NUM}"
EXERCISE_MODULE="Exercise${EXERCISE_NUM}"

TEST_DIR="test"
TEST_MAIN_PATH="${TEST_DIR}/Main.purs"
PARENT_DIR="${TEST_DIR}/${CHAPTER_MODULE}"
PARENT_SPEC_PATH="${PARENT_DIR}/Spec.purs"
EXERCISE_PATH="${PARENT_DIR}/${EXERCISE_MODULE}${EXERCISE_EXT}"

if [ ! -d "${PARENT_DIR}" ]; then
  echo "Creating ${PARENT_DIR} since it doesn't exist."
  mkdir -p "${PARENT_DIR}"

  echo "Creating ${PARENT_DIR}'s spec."
  cat > $PARENT_SPEC_PATH << EOF
module Test.${CHAPTER_MODULE}.Spec where

import Prelude

-- import Test.${CHAPTER_MODULE}.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter ${CHAPTER_ARG}" do
  -- ExerciseX.spec
  pure unit
EOF

  echo "Adding parent spec to the main spec"
  TMP_FILE="${TEST_MAIN_PATH}.tmp"
  cat $TEST_MAIN_PATH | sed "
    s/-- import Test.ChapterX.Spec as ChapterX/import Test.${CHAPTER_MODULE}.Spec as ${CHAPTER_MODULE}\n-- import Test.ChapterX.Spec as ChapterX/
    s/-- ChapterX.spec/  ${CHAPTER_MODULE}.spec\n-- ChapterX.spec/
    " > $TMP_FILE
  mv $TMP_FILE $TEST_MAIN_PATH
fi

if [ ! -f "${EXERCISE_PATH}" ]; then
  if [ "${EXERCISE_EXT}" == ".purs" ]; then
    echo "Creating PureScript file at ${EXERCISE_PATH} since it doesn't exist."
    cat > $EXERCISE_PATH << EOF
module Test.${CHAPTER_MODULE}.${EXERCISE_MODULE} where

import Prelude

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Exercise ${EXERCISE_ARG}" do
  it "it should work" do
    pure unit
EOF

  echo "Adding exercise to the parent spec"
  TMP_FILE="${PARENT_SPEC_PATH}.tmp"
  cat $PARENT_SPEC_PATH | sed "
    s/-- import Test.${CHAPTER_MODULE}.ExerciseX as ExerciseX/import Test.${CHAPTER_MODULE}.${EXERCISE_MODULE} as ${EXERCISE_MODULE}\n-- import Test.${CHAPTER_MODULE}.ExerciseX as ExerciseX/
    s/-- ExerciseX.spec/${EXERCISE_MODULE}.spec\n-- ExerciseX.spec/
    " > $TMP_FILE
  mv $TMP_FILE $PARENT_SPEC_PATH
  else
    echo "Creating Markdown file at ${EXERCISE_PATH} since it doesn't exist."
    cat > $EXERCISE_PATH << EOF
# Exercise ${EXERCISE_ARG}

EOF
  fi
fi

