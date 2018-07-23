CREATE TABLE people (
     id         serial PRIMARY KEY
    --
    ,last_name  text    NOT NULL
    ,first_name text    
    ,sur_name   text   
);

CREATE TABLE people_attrs (
     id          serial
    ,people_id   integer     NOT NULL REFERENCES people (id)
    --
    ,name        varchar(50) NOT NULL
    --
    ,period_date date        NOT NULL
);
