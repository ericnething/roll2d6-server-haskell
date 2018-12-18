-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

-- create extension pgcrypto;

------------------------------------------------------------
-- Update updated_at field when an entry changes
------------------------------------------------------------

create or replace function update_timestamp_field()
 returns trigger as $$
begin
  new.updated_at = now();
  return new;
end;
$$ language 'plpgsql';

------------------------------------------------------------
-- Access Level
------------------------------------------------------------

create type access_level as enum (
  'player',
  'game_master',
  'owner'
);

------------------------------------------------------------
-- Person
------------------------------------------------------------

create table person (
  id         serial       primary key,
  username   varchar      not null,
  email      varchar      not null unique,
  password   varchar      not null,
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
  id         uuid         primary key default gen_random_uuid(),
  title      varchar      not null,
  gameType   varchar      not null,
  created_at timestamptz  not null default now(),
  updated_at timestamptz  not null default now()
);

create trigger update_game_timestamp
  before update on game
  for each row execute procedure update_timestamp_field();

------------------------------------------------------------
-- Person Game Relations
------------------------------------------------------------

create table person_game_relation (
  game_id    uuid         not null references game (id),
  person_id  integer      not null references person (id),
  access     access_level not null default 'player',
  created_at timestamptz  not null default now(),
  updated_at timestamptz  not null default now(),
  primary key (game_id, person_id)
);

create trigger update_person_game_relation_timestamp
  before update on person_game_relation
  for each row execute procedure update_timestamp_field();

------------------------------------------------------------
-- Chat and dice rolling messages
------------------------------------------------------------

create type chat_message_type as enum (
  'chat_message',
  'dice_roll'
);

create table chat_message (
  game_id     uuid         not null references game (id),
  person_id   integer      not null references person (id),
  ctor        chat_message_type not null,
  body        varchar,
  dice_result jsonb,
  created_at  timestamptz  not null default now(),
  updated_at  timestamptz  not null default now(),
  primary key (game_id, person_id, created_at),
  constraint chat_message_sum_type
      check (num_nulls(body, dice_result) = 1)
);

create trigger update_chat_message_timestamp
  before update on chat_message
  for each row execute procedure update_timestamp_field();


------------------------------------------------------------
-- Unique Sheet IDs
------------------------------------------------------------

create table game_sheet (
  game_id     uuid         not null references game (id),
  sheet_id    uuid         not null default gen_random_uuid(),
  primary key (game_id, sheet_id)
);
