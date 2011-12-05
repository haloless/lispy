
CREATE TABLE users (
       user_id integer PRIMARY KEY,
       username varchar(255) UNIQUE NOT NULL,
       password varchar(63) NOT NULL,
       
       user_type varchar(31) NOT NULL,
       
       ts_created datetime NOT NULL,
       ts_last_login datetime,
);

CREATE TABLE users_profile ();
