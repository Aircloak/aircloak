--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: user_data; Type: TABLE; Schema: public; Owner: cloak; Tablespace: 
--

CREATE TABLE user_data (
    id integer NOT NULL,
    user_id integer,
    item text,
    price real
);


ALTER TABLE user_data OWNER TO cloak;

--
-- Name: user_data_id_seq; Type: SEQUENCE; Schema: public; Owner: cloak
--

CREATE SEQUENCE user_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE user_data_id_seq OWNER TO cloak;

--
-- Name: user_data_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: cloak
--

ALTER SEQUENCE user_data_id_seq OWNED BY user_data.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: cloak
--

ALTER TABLE ONLY user_data ALTER COLUMN id SET DEFAULT nextval('user_data_id_seq'::regclass);


--
-- Data for Name: user_data; Type: TABLE DATA; Schema: public; Owner: cloak
--

INSERT INTO user_data (id, user_id, item, price) VALUES (1, 1, 'car', 1000);
INSERT INTO user_data (id, user_id, item, price) VALUES (2, 1, 'vacation', 500);
INSERT INTO user_data (id, user_id, item, price) VALUES (3, 1, 'food', 20.5);
INSERT INTO user_data (id, user_id, item, price) VALUES (4, 2, 'food', 10);
INSERT INTO user_data (id, user_id, item, price) VALUES (5, 2, 'car', 700);
INSERT INTO user_data (id, user_id, item, price) VALUES (6, 3, 'vacation', 100);
INSERT INTO user_data (id, user_id, item, price) VALUES (7, 4, 'vacation', 230);
INSERT INTO user_data (id, user_id, item, price) VALUES (8, 4, 'food', 21.7000008);
INSERT INTO user_data (id, user_id, item, price) VALUES (9, 5, 'movies', 12.1000004);
INSERT INTO user_data (id, user_id, item, price) VALUES (10, 6, 'drinks', 16);
INSERT INTO user_data (id, user_id, item, price) VALUES (11, 6, 'food', 8.19999981);
INSERT INTO user_data (id, user_id, item, price) VALUES (12, 7, 'car', 750);
INSERT INTO user_data (id, user_id, item, price) VALUES (13, 8, 'house', 5000);
INSERT INTO user_data (id, user_id, item, price) VALUES (14, 8, 'vacation', 300);
INSERT INTO user_data (id, user_id, item, price) VALUES (15, 8, 'food', 80.5);
INSERT INTO user_data (id, user_id, item, price) VALUES (16, 9, 'food', 50.5);
INSERT INTO user_data (id, user_id, item, price) VALUES (17, 9, 'drinks', 150.75);
INSERT INTO user_data (id, user_id, item, price) VALUES (18, 10, 'vacation', 250);
INSERT INTO user_data (id, user_id, item, price) VALUES (19, 11, 'movies', 20.7000008);
INSERT INTO user_data (id, user_id, item, price) VALUES (20, 11, 'drinks', 12.1000004);
INSERT INTO user_data (id, user_id, item, price) VALUES (21, 12, 'gym', 50);
INSERT INTO user_data (id, user_id, item, price) VALUES (22, 13, 'health', 1250);
INSERT INTO user_data (id, user_id, item, price) VALUES (23, 13, 'food', 32.5600014);
INSERT INTO user_data (id, user_id, item, price) VALUES (24, 14, 'drinks', 34);
INSERT INTO user_data (id, user_id, item, price) VALUES (25, 15, 'food', 64.0999985);
INSERT INTO user_data (id, user_id, item, price) VALUES (26, 15, 'vacation', 160);
INSERT INTO user_data (id, user_id, item, price) VALUES (27, 15, 'house', 6240);


--
-- Name: user_data_id_seq; Type: SEQUENCE SET; Schema: public; Owner: cloak
--

SELECT pg_catalog.setval('user_data_id_seq', 27, true);


--
-- Name: user_data; Type: ACL; Schema: public; Owner: cloak
--

REVOKE ALL ON TABLE user_data FROM PUBLIC;
REVOKE ALL ON TABLE user_data FROM postgres;
GRANT ALL ON TABLE user_data TO postgres;
GRANT ALL ON TABLE user_data TO cloak;


--
-- PostgreSQL database dump complete
--

