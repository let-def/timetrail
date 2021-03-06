export PACKS = unix grenier.hll inuit sturgeon sturgeon.recipes_command

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

define PROJ_timetrail_sturgeon
  SOURCES = common.ml timetrail_sturgeon.ml
  RESULT = timetrail-sturgeon
endef
export PROJ_timetrail_sturgeon

ifndef SUBPROJS
  export SUBPROJS = timetrail timetrail_compact timetrail_log timetrail_sturgeon
endif

all: native-code

%:
	@$(MAKE) -f OCamlMakefile subprojs SUBTARGET=$@
