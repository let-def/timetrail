export PACKS = unix grenier.hll

define PROJ_timetrail
  SOURCES = common.ml timetrail.ml
  RESULT = timetrail
endef
export PROJ_timetrail

define PROJ_timetrail_compact
  SOURCES = common.ml timetrail_compact.ml
  RESULT = timetrail-compact
endef
export PROJ_timetrail_compact

define PROJ_timetrail_log
  SOURCES = common.ml timetrail_log.ml
  RESULT = timetrail-log
endef
export PROJ_timetrail_log

ifndef SUBPROJS
  export SUBPROJS = timetrail timetrail_compact timetrail_log
endif

all: native-code

%:
	@$(MAKE) -f OCamlMakefile subprojs SUBTARGET=$@
