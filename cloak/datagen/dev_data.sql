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
    uid integer not null,
    user_name character varying(20),
    row_id integer not null,
    itemname text,
    price integer,
    date timestamp without time zone
);

ALTER TABLE public.purchases OWNER TO cloak;

--
-- Name: purchases_row_id_seq; Type: SEQUENCE; Schema: public; Owner: cloak
--

CREATE SEQUENCE purchases_row_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE purchases_row_id_seq OWNER TO cloak;

--
-- Name: purchases_row_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: cloak
--

ALTER SEQUENCE purchases_row_id_seq OWNED BY purchases.row_id;


--
-- Name: row_id; Type: DEFAULT; Schema: public; Owner: cloak
--

ALTER TABLE ONLY purchases ALTER COLUMN row_id SET DEFAULT nextval('purchases_row_id_seq'::regclass);

CREATE INDEX ON purchases (uid);
CREATE UNIQUE INDEX ON purchases (row_id);

--
-- Data for Name: purchases; Type: TABLE DATA; Schema: public; Owner: cloak
--

COPY purchases (uid, user_name, itemname, price, date) FROM stdin;
1	sebastian	toaster	10	2016-05-18 00:00:00
1	sebastian	toaster	11	2016-05-17 10:00:01
1	sebastian	toaster	10	2016-05-17 11:00:01
1	sebastian	smoothie	3	2016-05-16 05:00:00
1	sebastian	car	30000	2016-05-16 05:00:00
1	sebastian	castle	3000000	2016-02-10 15:00:00
1	sebastian	jam jar	2	2015-09-01 13:00:00
2	cristian	shampoo	5	2015-09-01 13:00:00
3	sasa	shampoo	5	2015-09-02 13:00:00
4	pascal	shampoo	5	2016-02-01 13:00:00
4	pascal	shampoo	5	2016-01-01 13:00:00
5	pawel	shampoo	5	2015-12-01 13:00:00
6	felix	shampoo	5	2015-12-12 13:00:00
6	felix	shampoo	5	2016-01-12 13:00:00
6	felix	shampoo	5	2016-02-12 13:00:00
6	felix	shampoo	5	2015-03-12 13:00:00
1	sebastian	toaster	11	2015-05-17 10:00:01
6	felix	toaster	10	2016-05-17 11:00:01
7	paul	smoothie	3	2016-05-16 05:00:00
7	paul	smoothie	3	2016-05-17 05:00:00
7	paul	smoothie	3	2016-05-18 05:00:00
7	paul	smoothie	3	2016-05-19 05:00:00
7	paul	smoothie	3	2016-05-20 05:00:00
7	paul	smoothie	3	2016-05-20 06:00:00
7	paul	smoothie	3	2016-05-20 07:00:00
7	paul	smoothie	3	2016-05-20 08:00:00
7	paul	smoothie	3	2016-05-20 09:00:00
7	paul	smoothie	3	2016-05-20 10:00:00
7	paul	smoothie	3	2016-05-16 05:00:00
8	mamiko	smoothie	3	2016-05-16 05:00:00
8	mamiko	smoothie	3	2016-05-17 05:00:00
8	mamiko	smoothie	3	2016-05-18 05:00:00
8	mamiko	smoothie	3	2016-05-19 05:00:00
8	mamiko	smoothie	3	2016-05-20 05:00:00
8	mamiko	smoothie	3	2016-05-20 06:00:00
8	mamiko	smoothie	3	2016-05-20 07:00:00
8	mamiko	smoothie	3	2016-05-20 08:00:00
8	mamiko	smoothie	3	2016-05-20 09:00:00
8	mamiko	smoothie	3	2016-05-20 10:00:00
2	cristian	smoothie	3	2015-05-20 07:00:00
2	cristian	smoothie	3	2015-05-20 08:00:00
2	cristian	smoothie	3	2015-05-20 09:00:00
2	cristian	smoothie	3	2015-05-20 10:00:00
3	sasa	car	35000	2016-04-20 10:00:00
4	pascal	car	40000	2016-04-21 10:00:00
7	paul	car	45000	2016-04-22 10:00:00
6	felix	car	50000	2016-04-23 10:00:00
2	cristian	car	55000	2016-04-24 10:00:00
5	pawel	car	100000	2016-04-25 10:00:00
1	sebastian	car	5000	2016-04-26 10:00:00
9	person1	lottery ticket	2	2016-04-26 10:00:00
10	person2	lottery ticket	2	2016-04-26 10:00:00
11	person3	lottery ticket	2	2016-04-26 10:00:00
12	person4	lottery ticket	2	2016-04-26 10:00:00
13	person5	lottery ticket	2	2016-04-26 10:00:00
14	person6	lottery ticket	2	2016-04-26 10:00:00
15	person7	lottery ticket	2	2016-04-26 10:00:00
16	person8	lottery ticket	2	2016-04-26 10:00:00
17	person9	lottery ticket	2	2016-04-26 10:00:00
18	person10	lottery ticket	2	2016-04-26 10:00:00
19	person11	lottery ticket	2	2016-04-26 10:00:00
20	person12	lottery ticket	2	2016-04-26 10:00:00
21	person13	lottery ticket	2	2016-04-26 10:00:00
22	person14	lottery ticket	2	2016-04-26 10:00:00
23	person15	lottery ticket	2	2016-04-26 10:00:00
21	person13	lottery ticket	2	2016-04-26 10:00:00
22	person14	lottery ticket	2	2016-04-26 10:00:00
23	person15	lottery ticket	2	2016-04-26 10:00:00
21	person13	lottery ticket	2	2016-04-26 10:00:00
22	person14	lottery ticket	2	2016-04-26 10:00:00
23	person15	lottery ticket	2	2016-04-26 10:00:00
21	person13	lottery ticket	2	2016-04-26 10:00:00
22	person14	lottery ticket	2	2016-04-26 10:00:00
23	person15	lottery ticket	2	2016-04-26 10:00:00
24	person16	lottery ticket	2	2016-04-26 10:00:00
25	person17	lottery ticket	2	2016-04-26 10:00:00
26	person18	lottery ticket	2	2016-04-26 10:00:00
27	person19	lottery ticket	2	2016-04-26 10:00:00
28	person20	lottery ticket	2	2016-04-26 10:00:00
28	person20	lottery ticket	4	2016-04-22 10:00:00
28	person20	lottery ticket	2	2016-04-23 10:00:00
28	person20	lottery ticket	2	2016-04-24 10:00:00
10	person1	gym membership	20	2016-04-26 10:00:00
11	person2	gym membership	20	2016-04-26 10:00:00
12	person3	gym membership	20	2016-04-26 10:00:00
13	person4	gym membership	20	2016-04-26 10:00:00
14	person5	gym membership	20	2016-04-26 10:00:00
15	person6	gym membership	20	2016-04-26 10:00:00
16	person7	gym membership	20	2016-04-26 10:00:00
17	person8	gym membership	20	2016-04-26 10:00:00
18	person9	gym membership	20	2016-04-26 10:00:00
19	person10	gym membership	20	2016-03-26 10:00:00
20	person11	gym membership	20	2016-03-26 10:00:00
21	person12	gym membership	20	2016-03-26 10:00:00
22	person13	gym membership	20	2016-03-26 10:00:00
23	person14	gym membership	20	2016-03-26 10:00:00
24	person15	gym membership	20	2016-03-26 10:00:00
22	person13	gym membership	20	2016-03-26 10:00:00
23	person14	gym membership	20	2016-03-26 10:00:00
24	person15	gym membership	20	2016-03-26 10:00:00
22	person13	gym membership	20	2016-03-26 10:00:00
23	person14	gym membership	20	2016-03-26 10:00:00
24	person15	gym membership	20	2016-03-26 10:00:00
22	person13	gym membership	20	2016-03-26 10:00:00
23	person14	gym membership	20	2016-03-26 10:00:00
23	person15	gym membership	20	2016-03-26 10:00:00
24	person16	gym membership	20	2016-03-26 10:00:00
25	person17	gym membership	20	2016-03-26 10:00:00
26	person18	gym membership	20	2016-03-26 10:00:00
27	person19	gym membership	20	2016-03-26 10:00:00
28	person20	gym membership	20	2016-03-26 10:00:00
28	person20	gym membership	4	2016-03-22 10:00:00
28	person20	gym membership	20	2016-03-23 10:00:00
28	person20	gym membership	20	2016-03-24 10:00:00
1	sebastian	toaster	11	2014-05-17 10:00:01
1	sebastian	toaster	10	2014-05-17 11:00:01
1	sebastian	smoothie	3	2014-05-16 05:00:00
1	sebastian	car	30000	2014-05-16 05:00:00
1	sebastian	castle	3000000	2014-02-10 15:00:00
1	sebastian	jam jar	2	2013-09-01 13:00:00
2	cristian	shampoo	5	2013-09-01 13:00:00
3	sasa	shampoo	5	2013-09-02 13:00:00
4	pascal	shampoo	5	2014-02-01 13:00:00
4	pascal	shampoo	5	2014-01-01 13:00:00
5	pawel	shampoo	5	2013-12-01 13:00:00
6	felix	shampoo	5	2013-12-12 13:00:00
6	felix	shampoo	5	2014-01-12 13:00:00
6	felix	shampoo	5	2014-02-12 13:00:00
6	felix	shampoo	5	2013-03-12 13:00:00
1	sebastian	toaster	11	2013-05-17 10:00:01
6	felix	toaster	10	2014-05-17 11:00:01
7	paul	smoothie	3	2014-05-16 05:00:00
7	paul	smoothie	3	2014-05-17 05:00:00
7	paul	smoothie	3	2014-05-18 05:00:00
7	paul	smoothie	3	2014-05-19 05:00:00
7	paul	smoothie	3	2014-05-20 05:00:00
7	paul	smoothie	3	2014-05-20 06:00:00
7	paul	smoothie	3	2014-05-20 07:00:00
7	paul	smoothie	3	2014-05-20 08:00:00
7	paul	smoothie	3	2014-05-20 09:00:00
7	paul	smoothie	3	2014-05-20 10:00:00
7	paul	smoothie	3	2014-05-16 05:00:00
8	mamiko	smoothie	3	2014-05-16 05:00:00
8	mamiko	smoothie	3	2014-05-17 05:00:00
8	mamiko	smoothie	3	2014-05-18 05:00:00
8	mamiko	smoothie	3	2014-05-19 05:00:00
8	mamiko	smoothie	3	2014-05-20 05:00:00
8	mamiko	smoothie	3	2014-05-20 06:00:00
8	mamiko	smoothie	3	2014-05-20 07:00:00
8	mamiko	smoothie	3	2014-05-20 08:00:00
8	mamiko	smoothie	3	2014-05-20 09:00:00
8	mamiko	smoothie	3	2014-05-20 10:00:00
2	cristian	smoothie	3	2013-05-20 07:00:00
2	cristian	smoothie	3	2013-05-20 08:00:00
2	cristian	smoothie	3	2013-05-20 09:00:00
2	cristian	smoothie	3	2013-05-20 10:00:00
3	sasa	car	35000	2014-04-20 10:00:00
4	pascal	car	40000	2014-04-21 10:00:00
7	paul	car	45000	2014-04-22 10:00:00
6	felix	car	50000	2014-04-23 10:00:00
2	cristian	car	55000	2014-04-24 10:00:00
5	pawel	car	100000	2014-04-25 10:00:00
1	sebastian	car	5000	2014-04-26 10:00:00
9	person1	lottery ticket	2	2014-04-26 10:00:00
10	person2	lottery ticket	2	2014-04-26 10:00:00
11	person3	lottery ticket	2	2014-04-26 10:00:00
12	person4	lottery ticket	2	2014-04-26 10:00:00
13	person5	lottery ticket	2	2014-04-26 10:00:00
14	person6	lottery ticket	2	2014-04-26 10:00:00
15	person7	lottery ticket	2	2014-04-26 10:00:00
16	person8	lottery ticket	2	2014-04-26 10:00:00
17	person9	lottery ticket	2	2014-04-26 10:00:00
18	person10	lottery ticket	2	2014-04-26 10:00:00
19	person11	lottery ticket	2	2014-04-26 10:00:00
20	person12	lottery ticket	2	2014-04-26 10:00:00
21	person13	lottery ticket	2	2014-04-26 10:00:00
23	person14	lottery ticket	2	2014-04-26 10:00:00
23	person15	lottery ticket	2	2014-04-26 10:00:00
21	person13	lottery ticket	2	2014-04-26 10:00:00
23	person14	lottery ticket	2	2014-04-26 10:00:00
23	person15	lottery ticket	2	2014-04-26 10:00:00
21	person13	lottery ticket	2	2014-04-26 10:00:00
23	person14	lottery ticket	2	2014-04-26 10:00:00
23	person15	lottery ticket	2	2014-04-26 10:00:00
21	person13	lottery ticket	2	2014-04-26 10:00:00
23	person14	lottery ticket	2	2014-04-26 10:00:00
23	person15	lottery ticket	2	2014-04-26 10:00:00
24	person16	lottery ticket	2	2014-04-26 10:00:00
25	person17	lottery ticket	2	2014-04-26 10:00:00
26	person18	lottery ticket	2	2014-04-26 10:00:00
27	person19	lottery ticket	2	2014-04-26 10:00:00
28	person20	lottery ticket	2	2014-04-26 10:00:00
28	person20	lottery ticket	4	2014-04-22 10:00:00
28	person20	lottery ticket	2	2014-04-23 10:00:00
28	person20	lottery ticket	2	2014-04-24 10:00:00
9	person1	gym membership	20	2014-04-26 10:00:00
10	person2	gym membership	20	2014-04-26 10:00:00
11	person3	gym membership	20	2014-04-26 10:00:00
12	person4	gym membership	20	2014-04-26 10:00:00
13	person5	gym membership	20	2014-04-26 10:00:00
14	person6	gym membership	20	2014-04-26 10:00:00
15	person7	gym membership	20	2014-04-26 10:00:00
16	person8	gym membership	20	2014-04-26 10:00:00
17	person9	gym membership	20	2014-04-26 10:00:00
18	person10	gym membership	20	2014-03-26 10:00:00
19	person11	gym membership	20	2014-03-26 10:00:00
20	person12	gym membership	20	2014-03-26 10:00:00
21	person13	gym membership	20	2014-03-26 10:00:00
23	person14	gym membership	20	2014-03-26 10:00:00
23	person15	gym membership	20	2014-03-26 10:00:00
21	person13	gym membership	20	2014-03-26 10:00:00
23	person14	gym membership	20	2014-03-26 10:00:00
23	person15	gym membership	20	2014-03-26 10:00:00
21	person13	gym membership	20	2014-03-26 10:00:00
23	person14	gym membership	20	2014-03-26 10:00:00
23	person15	gym membership	20	2014-03-26 10:00:00
21	person13	gym membership	20	2014-03-26 10:00:00
23	person14	gym membership	20	2014-03-26 10:00:00
23	person15	gym membership	20	2014-03-26 10:00:00
24	person16	gym membership	20	2014-03-26 10:00:00
25	person17	gym membership	20	2014-03-26 10:00:00
26	person18	gym membership	20	2014-03-26 10:00:00
27	person19	gym membership	20	2014-03-26 10:00:00
28	person20	gym membership	20	2014-03-26 10:00:00
28	person20	gym membership	4	2014-03-22 10:00:00
28	person20	gym membership	20	2014-03-23 10:00:00
28	person20	gym membership	20	2014-03-24 10:00:00
\.

REVOKE ALL ON TABLE purchases FROM PUBLIC;
REVOKE ALL ON TABLE purchases FROM postgres;
GRANT ALL ON TABLE purchases TO postgres;
GRANT ALL ON TABLE purchases TO cloak;

--
-- Name: demographics; Type: TABLE; Schema: public; Owner: cloak; Tablespace:
--

CREATE TABLE demographics (
    uid integer,
    name character varying(40),
    age integer,
    alive boolean
);

ALTER TABLE public.demographics OWNER TO cloak;

CREATE INDEX ON demographics (uid);

--
-- Data for Name: demographics; Type: TABLE DATA; Schema: public; Owner: cloak
--

COPY demographics (uid, name, age, alive) FROM stdin;
1	Sebastian	31	TRUE
2	Cristian	10	TRUE
3	Sasa	10	TRUE
4	Pascal	10	TRUE
5	Pawel	10	FALSE
6	Felix	29	TRUE
7	Paul	10	TRUE
8	Mamiko	10	TRUE
9	Fred	20	FALSE
10	Bob	25	FALSE
11	Lisa	15	FALSE
12	Laura	30	FALSE
13	Bob	45	FALSE
14	Bob	50	FALSE
15	George	60	FALSE
16	Lisa	25	FALSE
17	Steven	50	TRUE
18	Isabel	27	TRUE
19	Isabel	28	TRUE
20	Isabel	29	TRUE
21	Isabel	30	TRUE
22	Isabel	31	TRUE
23	Isabel	32	TRUE
24	Isabel	33	TRUE
25	Lisa	34	FALSE
26	Weronika	24	FALSE
27	Muni	28	FALSE
28	Melanie	27	FALSE
29	Elena	32	FALSE
30	Isabel	26	FALSE
31	Isabel	25	FALSE
\.

REVOKE ALL ON TABLE demographics FROM PUBLIC;
REVOKE ALL ON TABLE demographics FROM postgres;
GRANT ALL ON TABLE demographics TO postgres;
GRANT ALL ON TABLE demographics TO cloak;


-- Data to simulate an account use case

--
-- Name: customer; Type: TABLE; Schema: public; Owner: cloak; Tablespace:
--

CREATE TABLE customers (
    id integer NOT NULL
);

ALTER TABLE customers OWNER TO cloak;
CREATE SEQUENCE customers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE customers_id_seq OWNER TO cloak;
ALTER SEQUENCE customers_id_seq OWNED BY customers.id;
ALTER TABLE ONLY customers ALTER COLUMN id SET DEFAULT nextval('customers_id_seq'::regclass);

INSERT INTO customers (id) VALUES (1);
INSERT INTO customers (id) VALUES (2);
INSERT INTO customers (id) VALUES (3);
INSERT INTO customers (id) VALUES (4);
INSERT INTO customers (id) VALUES (5);
INSERT INTO customers (id) VALUES (6);
INSERT INTO customers (id) VALUES (7);
INSERT INTO customers (id) VALUES (8);
INSERT INTO customers (id) VALUES (9);
INSERT INTO customers (id) VALUES (10);


CREATE TABLE accounts (
    id integer NOT NULL,
    customer_id integer,
    account_number text,
    swift varchar(11)
);

ALTER TABLE accounts OWNER TO cloak;
CREATE SEQUENCE accounts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE accounts_id_seq OWNER TO cloak;
ALTER SEQUENCE accounts_id_seq OWNED BY accounts.id;
ALTER TABLE ONLY accounts ALTER COLUMN id SET DEFAULT nextval('accounts_id_seq'::regclass);

INSERT INTO accounts (customer_id, account_number, swift) values (1, 'NL04873206260270312838', 'ABNCNL2AENE');
INSERT INTO accounts (customer_id, account_number, swift) values (2, 'GB1107265138028543504015', 'SOGEGB22GBS');
INSERT INTO accounts (customer_id, account_number, swift) values (3, 'DE60864644672838081844070', 'ABNADEFFBER');
INSERT INTO accounts (customer_id, account_number, swift) values (4, 'NL110635457877705476567445252', 'RGVMNL2RTR2');
INSERT INTO accounts (customer_id, account_number, swift) values (5, 'DE31638812021143003830768', 'IRVTDEFX');
INSERT INTO accounts (customer_id, account_number, swift) values (6, 'DE47134410814843108138', 'IRVTDEFX');
INSERT INTO accounts (customer_id, account_number, swift) values (7, 'NL8620021671548880764', 'ABNCNL2AENE');
INSERT INTO accounts (customer_id, account_number, swift) values (8, 'GB421161687217187724706', 'ANTSGB2L');
INSERT INTO accounts (customer_id, account_number, swift) values (9, 'DE4711075686234780338352315', 'TEAMDE77');
INSERT INTO accounts (customer_id, account_number, swift) values (10, 'DE237533210138420667843027', 'ABNADEFFBER');


CREATE TABLE transactions (
    id integer NOT NULL,
    account_id integer,
    amount integer,
    date timestamp,
    description text
);

ALTER TABLE transactions OWNER TO cloak;
CREATE SEQUENCE transactions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE transactions_id_seq OWNER TO cloak;
ALTER SEQUENCE transactions_id_seq OWNED BY transactions.id;
ALTER TABLE ONLY transactions ALTER COLUMN id SET DEFAULT nextval('transactions_id_seq'::regclass);

INSERT INTO transactions (account_id, amount, date, description) values (1, -1331, '2016-12-23 23:59:57', 'xtWiMYnNbXWVrAUgTAaquXCPpYVMhhtDtDleAcrbAOdYTPLJ');
INSERT INTO transactions (account_id, amount, date, description) values (1, -324, '2016-12-28 14:26:8', 'qVgByrpnSGsfidSGSywusOMhaOSLOFMCa');
INSERT INTO transactions (account_id, amount, date, description) values (2, 18, '2016-12-12 17:45:40', 'iXNNMSAHSoVquGvS');
INSERT INTO transactions (account_id, amount, date, description) values (2, -1000, '2016-12-19 3:1:5', 'Amazon');
INSERT INTO transactions (account_id, amount, date, description) values (3, -121, '2016-12-2 4:19:39', 'Amazon');
INSERT INTO transactions (account_id, amount, date, description) values (4, 15, '2016-12-26 5:20:13', 'EdPeEXWNMfjPslavBmQSfnPCODsMwuGKEXmfpjAfoCTNqHUWu');
INSERT INTO transactions (account_id, amount, date, description) values (5, -196, '2016-12-23 17:42:44', 'Amazon');
INSERT INTO transactions (account_id, amount, date, description) values (5, -225, '2016-12-31 5:41:21', 'IKEA');
INSERT INTO transactions (account_id, amount, date, description) values (6, -49, '2016-12-31 21:56:41', 'XDyuYakpwdrFRKlXOjeCMdiXABdkHSvqyfwmrdEcvjuNOftyIOmJeLwpcwTpNktaawUyqBJhYlhBonoVBoMISla');
INSERT INTO transactions (account_id, amount, date, description) values (6, 0, '2016-12-6 16:34:20', 'yuqtWHUycSulXUTOw');
INSERT INTO transactions (account_id, amount, date, description) values (7, -1331, '2016-12-27 12:40:19', 'BTakOoPmbcUOvObTraBUrJakdBTTErLJQTSNVOjyPhawwgUkpGKlORhQwlRvkbUUTCWNhFnGnKaJ');
INSERT INTO transactions (account_id, amount, date, description) values (8, -17, '2016-12-17 21:12:43', 'nkxbWHjXssbTun');
INSERT INTO transactions (account_id, amount, date, description) values (9, -16, '2016-12-3 19:39:20', 'Amazon');
INSERT INTO transactions (account_id, amount, date, description) values (10, 1, '2016-12-30 16:50:53', 'cIejxImLIDtlO');


--
-- PostgreSQL database dump complete
--

