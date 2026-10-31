-- migrate:up
CREATE TABLE nodes (
  id          uuid        PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_id    uuid        NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  parent_id   uuid        REFERENCES nodes (id) ON DELETE CASCADE,
  kind        text        NOT NULL CHECK (kind IN ('folder', 'file')),
  name        text        NOT NULL,
  size_bytes  bigint,
  mime_type   text,
  storage_key uuid        UNIQUE,
  status      text        NOT NULL CHECK (status IN ('pending', 'ready')),
  created_at  timestamptz NOT NULL DEFAULT now(),
  updated_at  timestamptz NOT NULL DEFAULT now(),
  deleted_at  timestamptz
);

-- One live name per folder; the root (parent_id NULL) counts as one folder.
CREATE UNIQUE INDEX nodes_unique_live_name_idx
  ON nodes (owner_id, parent_id, name) NULLS NOT DISTINCT
  WHERE deleted_at IS NULL;

-- Foreign keys: owner_id leads this one ...
CREATE INDEX nodes_owner_id_parent_id_idx ON nodes (owner_id, parent_id);
-- ... and parent_id needs its own, for ON DELETE CASCADE from a parent.
CREATE INDEX nodes_parent_id_idx ON nodes (parent_id);

CREATE TRIGGER nodes_set_updated_at
  BEFORE UPDATE ON nodes
  FOR EACH ROW EXECUTE FUNCTION set_updated_at();

-- migrate:down
DROP TABLE nodes;
