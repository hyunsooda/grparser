#!/bin/bash

set -ex

pushd ../../crates/grbuilder
cargo run "../../example/project-example"
popd
