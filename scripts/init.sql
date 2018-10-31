-- create extension pgcrypto;

-- Update update_at field when an entry changes
create or replace function update_timestamp_field()
 returns trigger as $$
begin
  new.updated_at = now();
  return new;
end;
$$ language 'plpgsql';

-- create type access_level as enum (
--   'customer',
--   'artist',
--   'admin'
-- );

------------------------------------------------------------
-- Person
------------------------------------------------------------

create table person (
  id         serial       primary key,
  uuid       uuid         not null default gen_random_uuid(),
  username   varchar      not null,
  email      varchar      not null unique,
  password   varchar      not null,
  -- access     access_level not null default 'customer',
  active     boolean      not null default true,
  created_at timestamptz  not null default now(),
  updated_at timestamptz  not null default now()
);

create trigger update_person_timestamp
  before update on person
  for each row execute procedure update_timestamp_field();

------------------------------------------------------------
-- Games
------------------------------------------------------------

create table game (
  id         serial       primary key,
  database   varchar      not null unique
                            default 'game_' || gen_random_uuid(),
  title      varchar      not null,
  person_id  integer      not null references person (id),
  created_at timestamptz  not null default now(),
  updated_at timestamptz  not null default now()
);

create trigger update_game_timestamp
  before update on game
  for each row execute procedure update_timestamp_field();

