-- Auto generated file, please don't modify.
-- To regenerate this file, make sure your aircloakdatabase is up to date, and
-- then run ./dump_db.sh

--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: activities; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE activities (
    id integer NOT NULL,
    user_id integer,
    description character varying(255),
    path character varying(255),
    success boolean,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: activities_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE activities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: activities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE activities_id_seq OWNED BY activities.id;


--
-- Name: alterations; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE alterations (
    id integer NOT NULL,
    target_type character varying(255),
    target_id integer,
    user_id integer,
    description text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: alterations_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE alterations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: alterations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE alterations_id_seq OWNED BY alterations.id;


--
-- Name: analyst_tokens; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE analyst_tokens (
    id integer NOT NULL,
    analyst_id integer NOT NULL,
    purpose integer NOT NULL,
    token text NOT NULL
);


--
-- Name: analyst_tokens_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE analyst_tokens_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: analyst_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE analyst_tokens_id_seq OWNED BY analyst_tokens.id;


--
-- Name: analysts; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE analysts (
    id integer NOT NULL,
    name character varying(255),
    key text,
    certificate text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    revocation_list text,
    admin_key text,
    admin_cert text,
    task_runner_key text,
    task_runner_cert text,
    web_api_key text,
    web_api_cert text
);


--
-- Name: analysts_clusters; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE analysts_clusters (
    cluster_id integer,
    analyst_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: analysts_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE analysts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: analysts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE analysts_id_seq OWNED BY analysts.id;


--
-- Name: audit_logs; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE audit_logs (
    id integer NOT NULL,
    cloak_id integer,
    log_message text,
    log_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: audit_logs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE audit_logs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: audit_logs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE audit_logs_id_seq OWNED BY audit_logs.id;


--
-- Name: buckets; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE buckets (
    id integer NOT NULL,
    result_id integer,
    label text,
    value text,
    count integer
);


--
-- Name: buckets_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE buckets_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: buckets_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE buckets_id_seq OWNED BY buckets.id;


--
-- Name: build_versions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE build_versions (
    id integer NOT NULL,
    deployable_entity_version_id integer,
    build_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: build_versions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE build_versions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: build_versions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE build_versions_id_seq OWNED BY build_versions.id;


--
-- Name: builds; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE builds (
    id integer NOT NULL,
    name character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    fingerprint character varying(255),
    build_completed boolean,
    build_success boolean,
    manual boolean
);


--
-- Name: builds_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE builds_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: builds_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE builds_id_seq OWNED BY builds.id;


--
-- Name: capabilities; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE capabilities (
    id integer NOT NULL,
    identifier character varying(255),
    name character varying(255),
    description text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: capabilities_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE capabilities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: capabilities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE capabilities_id_seq OWNED BY capabilities.id;


--
-- Name: capability_clusters; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE capability_clusters (
    id integer NOT NULL,
    cluster_id integer,
    capability_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: capability_clusters_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE capability_clusters_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: capability_clusters_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE capability_clusters_id_seq OWNED BY capability_clusters.id;


--
-- Name: cloaks; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cloaks (
    id integer NOT NULL,
    name character varying(255),
    ip character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    tpm boolean DEFAULT true,
    good boolean DEFAULT true
);


--
-- Name: cloaks_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE cloaks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: cloaks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE cloaks_id_seq OWNED BY cloaks.id;


--
-- Name: cluster_cloaks; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE cluster_cloaks (
    id integer NOT NULL,
    cluster_id integer,
    cloak_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    raw_state integer DEFAULT 1
);


--
-- Name: cluster_cloaks_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE cluster_cloaks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: cluster_cloaks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE cluster_cloaks_id_seq OWNED BY cluster_cloaks.id;


--
-- Name: clusters; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE clusters (
    id integer NOT NULL,
    name character varying(255),
    build_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    status_value integer DEFAULT 1,
    status_description character varying(255),
    last_modified timestamp without time zone,
    last_active timestamp without time zone DEFAULT '2015-03-09 08:56:02.41628'::timestamp without time zone
);


--
-- Name: clusters_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE clusters_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: clusters_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE clusters_id_seq OWNED BY clusters.id;


--
-- Name: deployable_entities; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE deployable_entities (
    id integer NOT NULL,
    repo character varying(255),
    description character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: deployable_entities_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE deployable_entities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: deployable_entities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE deployable_entities_id_seq OWNED BY deployable_entities.id;


--
-- Name: deployable_entity_versions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE deployable_entity_versions (
    id integer NOT NULL,
    deployable_entity_id integer,
    commit_id character varying(255),
    url character varying(255),
    message text,
    author character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    build_completed boolean,
    build_success boolean,
    build_log text
);


--
-- Name: deployable_entity_versions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE deployable_entity_versions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: deployable_entity_versions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE deployable_entity_versions_id_seq OWNED BY deployable_entity_versions.id;


--
-- Name: exception_results; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE exception_results (
    id integer NOT NULL,
    count integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    stacktrace text,
    result_id integer
);


--
-- Name: exception_results_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE exception_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: exception_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE exception_results_id_seq OWNED BY exception_results.id;


--
-- Name: key_materials; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE key_materials (
    id integer NOT NULL,
    analyst_id integer,
    description character varying(255),
    certificate text,
    key text,
    pkcs12 text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    revoked boolean DEFAULT false,
    key_type character varying(255),
    pem text,
    analyst_token_id integer
);


--
-- Name: key_materials_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE key_materials_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: key_materials_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE key_materials_id_seq OWNED BY key_materials.id;


--
-- Name: lookup_tables; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE lookup_tables (
    id integer NOT NULL,
    table_name character varying(255) NOT NULL,
    deleted boolean DEFAULT false NOT NULL,
    cluster_id integer NOT NULL,
    analyst_id integer NOT NULL,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: lookup_tables_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE lookup_tables_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: lookup_tables_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE lookup_tables_id_seq OWNED BY lookup_tables.id;


--
-- Name: pending_results; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE pending_results (
    id integer NOT NULL,
    auth_token character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    task_id integer,
    standing boolean DEFAULT false NOT NULL
);


--
-- Name: pending_results_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE pending_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pending_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE pending_results_id_seq OWNED BY pending_results.id;


--
-- Name: permissions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE permissions (
    id integer NOT NULL,
    name character varying(255),
    description character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE permissions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: permissions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE permissions_id_seq OWNED BY permissions.id;


--
-- Name: ra_library_code_ra_task_codes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE ra_library_code_ra_task_codes (
    id integer NOT NULL,
    ra_library_code_id integer,
    ra_task_code_id integer,
    name character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: ra_library_code_ra_task_codes_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE ra_library_code_ra_task_codes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ra_library_code_ra_task_codes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE ra_library_code_ra_task_codes_id_seq OWNED BY ra_library_code_ra_task_codes.id;


--
-- Name: ra_library_codes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE ra_library_codes (
    id integer NOT NULL,
    code text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: ra_library_codes_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE ra_library_codes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ra_library_codes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE ra_library_codes_id_seq OWNED BY ra_library_codes.id;


--
-- Name: ra_task_code_clusters; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE ra_task_code_clusters (
    id integer NOT NULL,
    ra_task_code_id integer,
    cluster_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: ra_task_code_clusters_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE ra_task_code_clusters_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ra_task_code_clusters_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE ra_task_code_clusters_id_seq OWNED BY ra_task_code_clusters.id;


--
-- Name: ra_task_codes; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE ra_task_codes (
    id integer NOT NULL,
    prefetch text,
    code text,
    trustworthy boolean DEFAULT false,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: ra_task_codes_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE ra_task_codes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ra_task_codes_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE ra_task_codes_id_seq OWNED BY ra_task_codes.id;


--
-- Name: repeated_answers; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE repeated_answers (
    id integer NOT NULL,
    analyst_id integer,
    bucket_label character varying(255),
    bucket_value character varying(255),
    bucket_count integer,
    "timestamp" integer,
    source_ip character varying(255),
    noise_sd double precision,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    resolved boolean,
    cluster_id integer
);


--
-- Name: repeated_answers_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE repeated_answers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: repeated_answers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE repeated_answers_id_seq OWNED BY repeated_answers.id;


--
-- Name: results; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE results (
    id integer NOT NULL,
    task_id integer,
    created_at timestamp without time zone,
    analyst_id integer,
    buckets_json text
);


--
-- Name: results_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE results_id_seq OWNED BY results.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: sessions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE sessions (
    id integer NOT NULL,
    session_id character varying(255),
    data text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: sessions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE sessions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sessions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE sessions_id_seq OWNED BY sessions.id;


--
-- Name: task_libraries; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE task_libraries (
    id integer NOT NULL,
    name character varying(255),
    code text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: task_libraries_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE task_libraries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: task_libraries_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE task_libraries_id_seq OWNED BY task_libraries.id;


--
-- Name: tasks; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tasks (
    id integer NOT NULL,
    update_task boolean DEFAULT false,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    name character varying(255),
    sandbox_type character varying(255) DEFAULT 'lua'::character varying,
    stored_task boolean DEFAULT false,
    cluster_id integer,
    code text,
    prefetch text,
    analyst_id integer,
    task_type integer DEFAULT 1 NOT NULL,
    report_interval integer,
    user_expire_interval integer,
    token text NOT NULL,
    test_data text,
    period text,
    active boolean DEFAULT true,
    code_timestamp timestamp without time zone DEFAULT '2015-03-11 08:43:19.736805'::timestamp without time zone,
    deleted boolean DEFAULT false NOT NULL,
    purged boolean DEFAULT false NOT NULL
);


--
-- Name: tasks_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tasks_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tasks_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tasks_id_seq OWNED BY tasks.id;


--
-- Name: test_item_vms; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE test_item_vms (
    id integer NOT NULL,
    test_item_id integer,
    test_vm_id integer,
    cpus integer,
    memory_size integer,
    memory_usage integer,
    disk_usage integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    name character varying(255),
    log text
);


--
-- Name: test_item_vms_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE test_item_vms_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: test_item_vms_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE test_item_vms_id_seq OWNED BY test_item_vms.id;


--
-- Name: test_items; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE test_items (
    id integer NOT NULL,
    test_result_id integer,
    name character varying(255),
    success boolean,
    duration integer,
    log text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: test_items_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE test_items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: test_items_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE test_items_id_seq OWNED BY test_items.id;


--
-- Name: test_results; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE test_results (
    id integer NOT NULL,
    testtime integer,
    test_server_version character varying(255),
    duration integer,
    benchmark_success boolean,
    benchmark_duration integer,
    benchmark_coverage double precision,
    benchmark_memory_usage integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: test_results_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE test_results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: test_results_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE test_results_id_seq OWNED BY test_results.id;


--
-- Name: test_vms; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE test_vms (
    id integer NOT NULL,
    test_result_id integer,
    name character varying(255),
    success boolean,
    duration integer,
    disk_size integer,
    disk_usage integer,
    log text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: test_vms_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE test_vms_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: test_vms_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE test_vms_id_seq OWNED BY test_vms.id;


--
-- Name: user_permissions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE user_permissions (
    id integer NOT NULL,
    user_id integer,
    permission_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


--
-- Name: user_permissions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE user_permissions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_permissions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE user_permissions_id_seq OWNED BY user_permissions.id;


--
-- Name: user_table_migrations; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE user_table_migrations (
    id integer NOT NULL,
    version integer,
    migration text,
    user_table_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    migrated boolean DEFAULT false,
    table_json text
);


--
-- Name: user_table_migrations_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE user_table_migrations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_table_migrations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE user_table_migrations_id_seq OWNED BY user_table_migrations.id;


--
-- Name: user_tables; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE user_tables (
    id integer NOT NULL,
    table_name character varying(255),
    cluster_id integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    deleted boolean DEFAULT false,
    pending_delete boolean DEFAULT false,
    analyst_id integer
);


--
-- Name: user_tables_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE user_tables_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_tables_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE user_tables_id_seq OWNED BY user_tables.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    login character varying(255),
    email character varying(255),
    crypted_password character varying(255),
    password_salt character varying(255),
    persistence_token character varying(255),
    single_access_token character varying(255),
    perishable_token character varying(255),
    login_count integer,
    failed_login_count integer,
    last_request_at timestamp without time zone,
    current_login_at timestamp without time zone,
    last_login_at timestamp without time zone,
    current_login_ip character varying(255),
    last_login_ip character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    analyst_id integer,
    activity_monitoring_opt_out boolean DEFAULT false
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY activities ALTER COLUMN id SET DEFAULT nextval('activities_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY alterations ALTER COLUMN id SET DEFAULT nextval('alterations_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY analyst_tokens ALTER COLUMN id SET DEFAULT nextval('analyst_tokens_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY analysts ALTER COLUMN id SET DEFAULT nextval('analysts_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY audit_logs ALTER COLUMN id SET DEFAULT nextval('audit_logs_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY buckets ALTER COLUMN id SET DEFAULT nextval('buckets_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY build_versions ALTER COLUMN id SET DEFAULT nextval('build_versions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY builds ALTER COLUMN id SET DEFAULT nextval('builds_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY capabilities ALTER COLUMN id SET DEFAULT nextval('capabilities_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY capability_clusters ALTER COLUMN id SET DEFAULT nextval('capability_clusters_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY cloaks ALTER COLUMN id SET DEFAULT nextval('cloaks_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY cluster_cloaks ALTER COLUMN id SET DEFAULT nextval('cluster_cloaks_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY clusters ALTER COLUMN id SET DEFAULT nextval('clusters_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY deployable_entities ALTER COLUMN id SET DEFAULT nextval('deployable_entities_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY deployable_entity_versions ALTER COLUMN id SET DEFAULT nextval('deployable_entity_versions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY exception_results ALTER COLUMN id SET DEFAULT nextval('exception_results_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY key_materials ALTER COLUMN id SET DEFAULT nextval('key_materials_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY lookup_tables ALTER COLUMN id SET DEFAULT nextval('lookup_tables_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY pending_results ALTER COLUMN id SET DEFAULT nextval('pending_results_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY permissions ALTER COLUMN id SET DEFAULT nextval('permissions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY ra_library_code_ra_task_codes ALTER COLUMN id SET DEFAULT nextval('ra_library_code_ra_task_codes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY ra_library_codes ALTER COLUMN id SET DEFAULT nextval('ra_library_codes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY ra_task_code_clusters ALTER COLUMN id SET DEFAULT nextval('ra_task_code_clusters_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY ra_task_codes ALTER COLUMN id SET DEFAULT nextval('ra_task_codes_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY repeated_answers ALTER COLUMN id SET DEFAULT nextval('repeated_answers_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY results ALTER COLUMN id SET DEFAULT nextval('results_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY sessions ALTER COLUMN id SET DEFAULT nextval('sessions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY task_libraries ALTER COLUMN id SET DEFAULT nextval('task_libraries_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tasks ALTER COLUMN id SET DEFAULT nextval('tasks_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY test_item_vms ALTER COLUMN id SET DEFAULT nextval('test_item_vms_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY test_items ALTER COLUMN id SET DEFAULT nextval('test_items_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY test_results ALTER COLUMN id SET DEFAULT nextval('test_results_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY test_vms ALTER COLUMN id SET DEFAULT nextval('test_vms_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY user_permissions ALTER COLUMN id SET DEFAULT nextval('user_permissions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY user_table_migrations ALTER COLUMN id SET DEFAULT nextval('user_table_migrations_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY user_tables ALTER COLUMN id SET DEFAULT nextval('user_tables_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Name: activities_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY activities
    ADD CONSTRAINT activities_pkey PRIMARY KEY (id);


--
-- Name: alterations_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY alterations
    ADD CONSTRAINT alterations_pkey PRIMARY KEY (id);


--
-- Name: analyst_table_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_table_migrations
    ADD CONSTRAINT analyst_table_migrations_pkey PRIMARY KEY (id);


--
-- Name: analyst_tables_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_tables
    ADD CONSTRAINT analyst_tables_pkey PRIMARY KEY (id);


--
-- Name: analyst_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY analyst_tokens
    ADD CONSTRAINT analyst_tokens_pkey PRIMARY KEY (id);


--
-- Name: analysts_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY analysts
    ADD CONSTRAINT analysts_pkey PRIMARY KEY (id);


--
-- Name: audit_logs_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY audit_logs
    ADD CONSTRAINT audit_logs_pkey PRIMARY KEY (id);


--
-- Name: buckets_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY buckets
    ADD CONSTRAINT buckets_pkey PRIMARY KEY (id);


--
-- Name: build_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY build_versions
    ADD CONSTRAINT build_versions_pkey PRIMARY KEY (id);


--
-- Name: builds_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY builds
    ADD CONSTRAINT builds_pkey PRIMARY KEY (id);


--
-- Name: capabilities_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY capabilities
    ADD CONSTRAINT capabilities_pkey PRIMARY KEY (id);


--
-- Name: capability_clusters_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY capability_clusters
    ADD CONSTRAINT capability_clusters_pkey PRIMARY KEY (id);


--
-- Name: cloaks_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cloaks
    ADD CONSTRAINT cloaks_pkey PRIMARY KEY (id);


--
-- Name: cluster_cloaks_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY cluster_cloaks
    ADD CONSTRAINT cluster_cloaks_pkey PRIMARY KEY (id);


--
-- Name: clusters_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY clusters
    ADD CONSTRAINT clusters_pkey PRIMARY KEY (id);


--
-- Name: deployable_entities_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY deployable_entities
    ADD CONSTRAINT deployable_entities_pkey PRIMARY KEY (id);


--
-- Name: deployable_entity_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY deployable_entity_versions
    ADD CONSTRAINT deployable_entity_versions_pkey PRIMARY KEY (id);


--
-- Name: exception_results_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY exception_results
    ADD CONSTRAINT exception_results_pkey PRIMARY KEY (id);


--
-- Name: key_materials_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY key_materials
    ADD CONSTRAINT key_materials_pkey PRIMARY KEY (id);


--
-- Name: lookup_tables_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY lookup_tables
    ADD CONSTRAINT lookup_tables_pkey PRIMARY KEY (id);


--
-- Name: pending_results_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY pending_results
    ADD CONSTRAINT pending_results_pkey PRIMARY KEY (id);


--
-- Name: permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY permissions
    ADD CONSTRAINT permissions_pkey PRIMARY KEY (id);


--
-- Name: ra_library_code_ra_task_codes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY ra_library_code_ra_task_codes
    ADD CONSTRAINT ra_library_code_ra_task_codes_pkey PRIMARY KEY (id);


--
-- Name: ra_library_codes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY ra_library_codes
    ADD CONSTRAINT ra_library_codes_pkey PRIMARY KEY (id);


--
-- Name: ra_task_code_clusters_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY ra_task_code_clusters
    ADD CONSTRAINT ra_task_code_clusters_pkey PRIMARY KEY (id);


--
-- Name: ra_task_codes_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY ra_task_codes
    ADD CONSTRAINT ra_task_codes_pkey PRIMARY KEY (id);


--
-- Name: repeated_answers_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY repeated_answers
    ADD CONSTRAINT repeated_answers_pkey PRIMARY KEY (id);


--
-- Name: results_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY results
    ADD CONSTRAINT results_pkey PRIMARY KEY (id);


--
-- Name: sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (id);


--
-- Name: task_libraries_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY task_libraries
    ADD CONSTRAINT task_libraries_pkey PRIMARY KEY (id);


--
-- Name: tasks_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tasks
    ADD CONSTRAINT tasks_pkey PRIMARY KEY (id);


--
-- Name: test_item_vms_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY test_item_vms
    ADD CONSTRAINT test_item_vms_pkey PRIMARY KEY (id);


--
-- Name: test_items_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY test_items
    ADD CONSTRAINT test_items_pkey PRIMARY KEY (id);


--
-- Name: test_results_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY test_results
    ADD CONSTRAINT test_results_pkey PRIMARY KEY (id);


--
-- Name: test_vms_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY test_vms
    ADD CONSTRAINT test_vms_pkey PRIMARY KEY (id);


--
-- Name: user_permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_permissions
    ADD CONSTRAINT user_permissions_pkey PRIMARY KEY (id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: index_activities_on_user_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_activities_on_user_id ON activities USING btree (user_id);


--
-- Name: index_alterations_on_user_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_alterations_on_user_id ON alterations USING btree (user_id);


--
-- Name: index_analyst_tokens_on_token; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX index_analyst_tokens_on_token ON analyst_tokens USING btree (token);


--
-- Name: index_analysts_clusters_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_analysts_clusters_on_analyst_id ON analysts_clusters USING btree (analyst_id);


--
-- Name: index_analysts_clusters_on_cluster_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_analysts_clusters_on_cluster_id ON analysts_clusters USING btree (cluster_id);


--
-- Name: index_analysts_clusters_on_cluster_id_and_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX index_analysts_clusters_on_cluster_id_and_analyst_id ON analysts_clusters USING btree (cluster_id, analyst_id);


--
-- Name: index_build_versions_on_build_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_build_versions_on_build_id ON build_versions USING btree (build_id);


--
-- Name: index_build_versions_on_deployable_entity_version_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_build_versions_on_deployable_entity_version_id ON build_versions USING btree (deployable_entity_version_id);


--
-- Name: index_clusters_on_build_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_clusters_on_build_id ON clusters USING btree (build_id);


--
-- Name: index_deployable_entity_versions_on_deployable_entity_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_deployable_entity_versions_on_deployable_entity_id ON deployable_entity_versions USING btree (deployable_entity_id);


--
-- Name: index_exception_results_on_count; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_exception_results_on_count ON exception_results USING btree (count);


--
-- Name: index_exception_results_on_result_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_exception_results_on_result_id ON exception_results USING btree (result_id);


--
-- Name: index_lookup_tables_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_lookup_tables_on_analyst_id ON lookup_tables USING btree (analyst_id);


--
-- Name: index_lookup_tables_on_cluster_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_lookup_tables_on_cluster_id ON lookup_tables USING btree (cluster_id);


--
-- Name: index_pending_results_on_task_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_pending_results_on_task_id ON pending_results USING btree (task_id);


--
-- Name: index_ra_library_code_ra_task_codes_on_ra_library_code_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_ra_library_code_ra_task_codes_on_ra_library_code_id ON ra_library_code_ra_task_codes USING btree (ra_library_code_id);


--
-- Name: index_ra_library_code_ra_task_codes_on_ra_task_code_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_ra_library_code_ra_task_codes_on_ra_task_code_id ON ra_library_code_ra_task_codes USING btree (ra_task_code_id);


--
-- Name: index_ra_task_code_clusters_on_cluster_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_ra_task_code_clusters_on_cluster_id ON ra_task_code_clusters USING btree (cluster_id);


--
-- Name: index_ra_task_code_clusters_on_ra_task_code_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_ra_task_code_clusters_on_ra_task_code_id ON ra_task_code_clusters USING btree (ra_task_code_id);


--
-- Name: index_repeated_answers_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_repeated_answers_on_analyst_id ON repeated_answers USING btree (analyst_id);


--
-- Name: index_repeated_answers_on_cluster_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_repeated_answers_on_cluster_id ON repeated_answers USING btree (cluster_id);


--
-- Name: index_results_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_results_on_analyst_id ON results USING btree (analyst_id);


--
-- Name: index_results_on_task_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_results_on_task_id ON results USING btree (task_id);


--
-- Name: index_sessions_on_session_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_sessions_on_session_id ON sessions USING btree (session_id);


--
-- Name: index_sessions_on_updated_at; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_sessions_on_updated_at ON sessions USING btree (updated_at);


--
-- Name: index_tasks_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_tasks_on_analyst_id ON tasks USING btree (analyst_id);


--
-- Name: index_tasks_on_cluster_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_tasks_on_cluster_id ON tasks USING btree (cluster_id);


--
-- Name: index_tasks_on_token; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX index_tasks_on_token ON tasks USING btree (token);


--
-- Name: index_test_item_vms_on_test_item_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_test_item_vms_on_test_item_id ON test_item_vms USING btree (test_item_id);


--
-- Name: index_test_item_vms_on_test_vm_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_test_item_vms_on_test_vm_id ON test_item_vms USING btree (test_vm_id);


--
-- Name: index_test_items_on_test_result_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_test_items_on_test_result_id ON test_items USING btree (test_result_id);


--
-- Name: index_test_vms_on_test_result_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_test_vms_on_test_result_id ON test_vms USING btree (test_result_id);


--
-- Name: index_user_permissions_on_permission_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_user_permissions_on_permission_id ON user_permissions USING btree (permission_id);


--
-- Name: index_user_permissions_on_user_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_user_permissions_on_user_id ON user_permissions USING btree (user_id);


--
-- Name: index_user_table_migrations_on_user_table_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_user_table_migrations_on_user_table_id ON user_table_migrations USING btree (user_table_id);


--
-- Name: index_user_tables_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_user_tables_on_analyst_id ON user_tables USING btree (analyst_id);


--
-- Name: index_user_tables_on_cluster_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_user_tables_on_cluster_id ON user_tables USING btree (cluster_id);


--
-- Name: index_users_on_analyst_id; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_users_on_analyst_id ON users USING btree (analyst_id);


--
-- Name: index_users_on_email; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_users_on_email ON users USING btree (email);


--
-- Name: index_users_on_last_request_at; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_users_on_last_request_at ON users USING btree (last_request_at);


--
-- Name: index_users_on_login; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_users_on_login ON users USING btree (login);


--
-- Name: index_users_on_persistence_token; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE INDEX index_users_on_persistence_token ON users USING btree (persistence_token);


--
-- Name: unique_schema_migrations; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX unique_schema_migrations ON schema_migrations USING btree (version);


--
-- PostgreSQL database dump complete
--

