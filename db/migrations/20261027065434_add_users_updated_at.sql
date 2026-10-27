-- migrate:up
ALTER TABLE users ADD COLUMN updated_at timestamptz NOT NULL DEFAULT now();
UPDATE users SET updated_at = created_at;

-- migrate:down
ALTER TABLE users DROP COLUMN updated_at;
