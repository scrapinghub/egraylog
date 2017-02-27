PROJECT = egraylog
PROJECT_DESCRIPTION = graylog logger
PROJECT_VERSION = 0.2.3

ERLC_OPTS = -W0
SHELL_OPTS = -s egraylog_app -config config/devel.config

LOCAL_DEPS = sasl ssl crypto

DEPS = jiffy
dep_jiffy = git https://github.com/davisp/jiffy 0.14.8


EUNIT_OPTS = verbose


all::


devel: all shell


test: eunit


.PHONY: devel build test


include erlang.mk
