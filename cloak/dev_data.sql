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
-- Name: purchases; Type: TABLE; Schema: public; Owner: cloak; Tablespace:
--

CREATE TABLE purchases (
    uid character varying(20),
    itemname text,
    price integer,
    date timestamp without time zone
);

CREATE INDEX ON purchases (uid);

ALTER TABLE public.purchases OWNER TO cloak;

--
-- Data for Name: purchases; Type: TABLE DATA; Schema: public; Owner: seb
--

COPY purchases (uid, itemname, price, date) FROM stdin;
sebastian	toaster	10	2016-05-18 00:00:00
sebastian	toaster	11	2016-05-17 10:00:01
sebastian	toaster	10	2016-05-17 11:00:01
sebastian	smoothie	3	2016-05-16 05:00:00
sebastian	car	30000	2016-05-16 05:00:00
sebastian	castle	3000000	2016-02-10 15:00:00
sebastian	jam jar	2	2015-09-01 13:00:00
cristian	shampoo	5	2015-09-01 13:00:00
sasa	shampoo	5	2015-09-02 13:00:00
pascal	shampoo	5	2016-02-01 13:00:00
pascal	shampoo	5	2016-01-01 13:00:00
pawel	shampoo	5	2015-12-01 13:00:00
felix	shampoo	5	2015-12-12 13:00:00
felix	shampoo	5	2016-01-12 13:00:00
felix	shampoo	5	2016-02-12 13:00:00
felix	shampoo	5	2015-03-12 13:00:00
sebastian	toaster	11	2015-05-17 10:00:01
felix	toaster	10	2016-05-17 11:00:01
paul	smoothie	3	2016-05-16 05:00:00
paul	smoothie	3	2016-05-17 05:00:00
paul	smoothie	3	2016-05-18 05:00:00
paul	smoothie	3	2016-05-19 05:00:00
paul	smoothie	3	2016-05-20 05:00:00
paul	smoothie	3	2016-05-20 06:00:00
paul	smoothie	3	2016-05-20 07:00:00
paul	smoothie	3	2016-05-20 08:00:00
paul	smoothie	3	2016-05-20 09:00:00
paul	smoothie	3	2016-05-20 10:00:00
paul	smoothie	3	2016-05-16 05:00:00
mamiko	smoothie	3	2016-05-16 05:00:00
mamiko	smoothie	3	2016-05-17 05:00:00
mamiko	smoothie	3	2016-05-18 05:00:00
mamiko	smoothie	3	2016-05-19 05:00:00
mamiko	smoothie	3	2016-05-20 05:00:00
mamiko	smoothie	3	2016-05-20 06:00:00
mamiko	smoothie	3	2016-05-20 07:00:00
mamiko	smoothie	3	2016-05-20 08:00:00
mamiko	smoothie	3	2016-05-20 09:00:00
mamiko	smoothie	3	2016-05-20 10:00:00
cristian	smoothie	3	2015-05-20 07:00:00
cristian	smoothie	3	2015-05-20 08:00:00
cristian	smoothie	3	2015-05-20 09:00:00
cristian	smoothie	3	2015-05-20 10:00:00
sasa	car	35000	2016-04-20 10:00:00
pascal	car	40000	2016-04-21 10:00:00
paul	car	45000	2016-04-22 10:00:00
felix	car	50000	2016-04-23 10:00:00
cristian	car	55000	2016-04-24 10:00:00
pawel	car	100000	2016-04-25 10:00:00
sebastian	car	5000	2016-04-26 10:00:00
person1	lottery ticket	2	2016-04-26 10:00:00
person2	lottery ticket	2	2016-04-26 10:00:00
person3	lottery ticket	2	2016-04-26 10:00:00
person4	lottery ticket	2	2016-04-26 10:00:00
person5	lottery ticket	2	2016-04-26 10:00:00
person6	lottery ticket	2	2016-04-26 10:00:00
person7	lottery ticket	2	2016-04-26 10:00:00
person8	lottery ticket	2	2016-04-26 10:00:00
person9	lottery ticket	2	2016-04-26 10:00:00
person10	lottery ticket	2	2016-04-26 10:00:00
person11	lottery ticket	2	2016-04-26 10:00:00
person12	lottery ticket	2	2016-04-26 10:00:00
person13	lottery ticket	2	2016-04-26 10:00:00
person14	lottery ticket	2	2016-04-26 10:00:00
person15	lottery ticket	2	2016-04-26 10:00:00
person13	lottery ticket	2	2016-04-26 10:00:00
person14	lottery ticket	2	2016-04-26 10:00:00
person15	lottery ticket	2	2016-04-26 10:00:00
person13	lottery ticket	2	2016-04-26 10:00:00
person14	lottery ticket	2	2016-04-26 10:00:00
person15	lottery ticket	2	2016-04-26 10:00:00
person13	lottery ticket	2	2016-04-26 10:00:00
person14	lottery ticket	2	2016-04-26 10:00:00
person15	lottery ticket	2	2016-04-26 10:00:00
person16	lottery ticket	2	2016-04-26 10:00:00
person17	lottery ticket	2	2016-04-26 10:00:00
person18	lottery ticket	2	2016-04-26 10:00:00
person19	lottery ticket	2	2016-04-26 10:00:00
person20	lottery ticket	2	2016-04-26 10:00:00
person20	lottery ticket	4	2016-04-22 10:00:00
person20	lottery ticket	2	2016-04-23 10:00:00
person20	lottery ticket	2	2016-04-24 10:00:00
person1	gym membership	20	2016-04-26 10:00:00
person2	gym membership	20	2016-04-26 10:00:00
person3	gym membership	20	2016-04-26 10:00:00
person4	gym membership	20	2016-04-26 10:00:00
person5	gym membership	20	2016-04-26 10:00:00
person6	gym membership	20	2016-04-26 10:00:00
person7	gym membership	20	2016-04-26 10:00:00
person8	gym membership	20	2016-04-26 10:00:00
person9	gym membership	20	2016-04-26 10:00:00
person10	gym membership	20	2016-03-26 10:00:00
person11	gym membership	20	2016-03-26 10:00:00
person12	gym membership	20	2016-03-26 10:00:00
person13	gym membership	20	2016-03-26 10:00:00
person14	gym membership	20	2016-03-26 10:00:00
person15	gym membership	20	2016-03-26 10:00:00
person13	gym membership	20	2016-03-26 10:00:00
person14	gym membership	20	2016-03-26 10:00:00
person15	gym membership	20	2016-03-26 10:00:00
person13	gym membership	20	2016-03-26 10:00:00
person14	gym membership	20	2016-03-26 10:00:00
person15	gym membership	20	2016-03-26 10:00:00
person13	gym membership	20	2016-03-26 10:00:00
person14	gym membership	20	2016-03-26 10:00:00
person15	gym membership	20	2016-03-26 10:00:00
person16	gym membership	20	2016-03-26 10:00:00
person17	gym membership	20	2016-03-26 10:00:00
person18	gym membership	20	2016-03-26 10:00:00
person19	gym membership	20	2016-03-26 10:00:00
person20	gym membership	20	2016-03-26 10:00:00
person20	gym membership	4	2016-03-22 10:00:00
person20	gym membership	20	2016-03-23 10:00:00
person20	gym membership	20	2016-03-24 10:00:00
sebastian	toaster	11	2014-05-17 10:00:01
sebastian	toaster	10	2014-05-17 11:00:01
sebastian	smoothie	3	2014-05-16 05:00:00
sebastian	car	30000	2014-05-16 05:00:00
sebastian	castle	3000000	2014-02-10 15:00:00
sebastian	jam jar	2	2013-09-01 13:00:00
cristian	shampoo	5	2013-09-01 13:00:00
sasa	shampoo	5	2013-09-02 13:00:00
pascal	shampoo	5	2014-02-01 13:00:00
pascal	shampoo	5	2014-01-01 13:00:00
pawel	shampoo	5	2013-12-01 13:00:00
felix	shampoo	5	2013-12-12 13:00:00
felix	shampoo	5	2014-01-12 13:00:00
felix	shampoo	5	2014-02-12 13:00:00
felix	shampoo	5	2013-03-12 13:00:00
sebastian	toaster	11	2013-05-17 10:00:01
felix	toaster	10	2014-05-17 11:00:01
paul	smoothie	3	2014-05-16 05:00:00
paul	smoothie	3	2014-05-17 05:00:00
paul	smoothie	3	2014-05-18 05:00:00
paul	smoothie	3	2014-05-19 05:00:00
paul	smoothie	3	2014-05-20 05:00:00
paul	smoothie	3	2014-05-20 06:00:00
paul	smoothie	3	2014-05-20 07:00:00
paul	smoothie	3	2014-05-20 08:00:00
paul	smoothie	3	2014-05-20 09:00:00
paul	smoothie	3	2014-05-20 10:00:00
paul	smoothie	3	2014-05-16 05:00:00
mamiko	smoothie	3	2014-05-16 05:00:00
mamiko	smoothie	3	2014-05-17 05:00:00
mamiko	smoothie	3	2014-05-18 05:00:00
mamiko	smoothie	3	2014-05-19 05:00:00
mamiko	smoothie	3	2014-05-20 05:00:00
mamiko	smoothie	3	2014-05-20 06:00:00
mamiko	smoothie	3	2014-05-20 07:00:00
mamiko	smoothie	3	2014-05-20 08:00:00
mamiko	smoothie	3	2014-05-20 09:00:00
mamiko	smoothie	3	2014-05-20 10:00:00
cristian	smoothie	3	2013-05-20 07:00:00
cristian	smoothie	3	2013-05-20 08:00:00
cristian	smoothie	3	2013-05-20 09:00:00
cristian	smoothie	3	2013-05-20 10:00:00
sasa	car	35000	2014-04-20 10:00:00
pascal	car	40000	2014-04-21 10:00:00
paul	car	45000	2014-04-22 10:00:00
felix	car	50000	2014-04-23 10:00:00
cristian	car	55000	2014-04-24 10:00:00
pawel	car	100000	2014-04-25 10:00:00
sebastian	car	5000	2014-04-26 10:00:00
person1	lottery ticket	2	2014-04-26 10:00:00
person2	lottery ticket	2	2014-04-26 10:00:00
person3	lottery ticket	2	2014-04-26 10:00:00
person4	lottery ticket	2	2014-04-26 10:00:00
person5	lottery ticket	2	2014-04-26 10:00:00
person6	lottery ticket	2	2014-04-26 10:00:00
person7	lottery ticket	2	2014-04-26 10:00:00
person8	lottery ticket	2	2014-04-26 10:00:00
person9	lottery ticket	2	2014-04-26 10:00:00
person10	lottery ticket	2	2014-04-26 10:00:00
person11	lottery ticket	2	2014-04-26 10:00:00
person12	lottery ticket	2	2014-04-26 10:00:00
person13	lottery ticket	2	2014-04-26 10:00:00
person14	lottery ticket	2	2014-04-26 10:00:00
person15	lottery ticket	2	2014-04-26 10:00:00
person13	lottery ticket	2	2014-04-26 10:00:00
person14	lottery ticket	2	2014-04-26 10:00:00
person15	lottery ticket	2	2014-04-26 10:00:00
person13	lottery ticket	2	2014-04-26 10:00:00
person14	lottery ticket	2	2014-04-26 10:00:00
person15	lottery ticket	2	2014-04-26 10:00:00
person13	lottery ticket	2	2014-04-26 10:00:00
person14	lottery ticket	2	2014-04-26 10:00:00
person15	lottery ticket	2	2014-04-26 10:00:00
person16	lottery ticket	2	2014-04-26 10:00:00
person17	lottery ticket	2	2014-04-26 10:00:00
person18	lottery ticket	2	2014-04-26 10:00:00
person19	lottery ticket	2	2014-04-26 10:00:00
person20	lottery ticket	2	2014-04-26 10:00:00
person20	lottery ticket	4	2014-04-22 10:00:00
person20	lottery ticket	2	2014-04-23 10:00:00
person20	lottery ticket	2	2014-04-24 10:00:00
person1	gym membership	20	2014-04-26 10:00:00
person2	gym membership	20	2014-04-26 10:00:00
person3	gym membership	20	2014-04-26 10:00:00
person4	gym membership	20	2014-04-26 10:00:00
person5	gym membership	20	2014-04-26 10:00:00
person6	gym membership	20	2014-04-26 10:00:00
person7	gym membership	20	2014-04-26 10:00:00
person8	gym membership	20	2014-04-26 10:00:00
person9	gym membership	20	2014-04-26 10:00:00
person10	gym membership	20	2014-03-26 10:00:00
person11	gym membership	20	2014-03-26 10:00:00
person12	gym membership	20	2014-03-26 10:00:00
person13	gym membership	20	2014-03-26 10:00:00
person14	gym membership	20	2014-03-26 10:00:00
person15	gym membership	20	2014-03-26 10:00:00
person13	gym membership	20	2014-03-26 10:00:00
person14	gym membership	20	2014-03-26 10:00:00
person15	gym membership	20	2014-03-26 10:00:00
person13	gym membership	20	2014-03-26 10:00:00
person14	gym membership	20	2014-03-26 10:00:00
person15	gym membership	20	2014-03-26 10:00:00
person13	gym membership	20	2014-03-26 10:00:00
person14	gym membership	20	2014-03-26 10:00:00
person15	gym membership	20	2014-03-26 10:00:00
person16	gym membership	20	2014-03-26 10:00:00
person17	gym membership	20	2014-03-26 10:00:00
person18	gym membership	20	2014-03-26 10:00:00
person19	gym membership	20	2014-03-26 10:00:00
person20	gym membership	20	2014-03-26 10:00:00
person20	gym membership	4	2014-03-22 10:00:00
person20	gym membership	20	2014-03-23 10:00:00
person20	gym membership	20	2014-03-24 10:00:00
\.

REVOKE ALL ON TABLE purchases FROM PUBLIC;
REVOKE ALL ON TABLE purchases FROM postgres;
GRANT ALL ON TABLE purchases TO postgres;
GRANT ALL ON TABLE purchases TO cloak;

--
-- PostgreSQL database dump complete
--

