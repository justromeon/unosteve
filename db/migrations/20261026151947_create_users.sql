-- migrate:up
CREATE TABLE users (
  id            uuid        PRIMARY KEY DEFAULT gen_random_uuid(),
  email         text        NOT NULL UNIQUE CHECK (email = lower(email)),
  password_hash text        NOT NULL,
  quota_bytes   bigint      NOT NULL CHECK (quota_bytes >= 0),
  created_at    timestamptz NOT NULL DEFAULT now()
);

-- migrate:down
DROP TABLE users;
