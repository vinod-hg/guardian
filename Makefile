# See LICENSE for licensing information.

PROJECT = guardian
PROJECT_DESCRIPTION = guard
PROJECT_VERSION = 0.1.1

# Compile options.
COMPILE_FIRST = guardian
ERLC_OPTS += +warn_export_all +warn_missing_spec +warn_untyped_record
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

# Dialyzer Options
DIALYZER_OPTS = -Werror_handling -Wunmatched_returns

# Standard targets.
include erlang.mk

# Generate rebar.config on build.
app:: rebar.config