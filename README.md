# UnoSteve

A file storage backend in Haskell: a JSON API for a personal drive with accounts,
folders and files. File bytes go directly between the client and object storage
(Cloudflare R2) through presigned URLs, so they never pass through the server.

> Work in progress: auth, folders and the storage layer are done. Uploads, downloads and
> quota are next.

## How it's built

| Aspect | Implementation |
|---|---|
| HTTP server | **Servant** on Warp, JSON only, versioned under `/api/v1` |
| Auth | **Server-side sessions**: an opaque bearer token of 32 random bytes from the OS (base64url). Only its **SHA-256** hash is stored. **7-day sliding** idle expiry (renewed past half its lifetime), **30-day absolute** limit, immediate logout and logout-all. Plugged in with Servant's generalized auth |
| Passwords | **argon2id** (`password` library); same `401` for unknown email and wrong password, with a dummy hash check against timing attacks |
| Database | **PostgreSQL** via **hasql** (binary protocol, SQL checked at compile time with hasql-th), **hasql-pool**; migrations in plain SQL with **dbmate** |
| Folders | Adjacency-list tree, unique names per folder (partial unique index), **keyset pagination** with opaque cursors. Every query is scoped to its owner: another user's node is a `404` |
| Object storage | **Cloudflare R2** over the S3 API. Upload URLs are signed by a small **SigV4** presigner of our own that signs `Content-Type` **and `Content-Length`**, so R2 refuses an upload of any other size before storing it (amazonka never signs the length). Downloads, HEAD and DELETE use **amazonka**; downloads carry an RFC 6266/8187 `Content-Disposition` |
| Architecture | **Handle pattern**: no app monad; each component is a record of its dependencies, built in `Main` |
| Errors | Expected failures are `Either` values with small per-operation types, mapped to HTTP once in the handlers; unexpected ones are exceptions (500) |
| Build & tooling | **Nix flakes** (haskell-flake), fourmolu, hlint, GitHub Actions CI, gitleaks |

### Testing

- **Fakes only through handles, and only where needed.** The clock (`Clock.Fixed`,
  `Clock.Settable`), object storage (`Storage.InMemory`) and logger (`Logger.Collecting`)
  are interfaces with test implementations. Postgres and argon2 are always real: argon2
  just runs with cheap parameters.
- **Database tests run against a real test database**, with empty tables for each test.
- **HTTP tests** use hspec-wai against the real application: status codes, headers,
  login/logout flows and IDOR checks.
- **R2 integration tests** run against the dev bucket, and only when you opt in.

## Development setup

- [Nix installation](https://nixos.org/download/)
- [Enable nix flakes](https://nixos.wiki/wiki/Flakes)
- Enter development shell

  ```bash
  nix develop
  ```

  With [direnv](https://direnv.net/), `direnv allow` enters it automatically and loads `.env`.

- Configure: copy `.env.example` to `.env` and fill in the R2 values (`S3_*`).

- Start Postgres and apply migrations (keep it running):

  ```bash
  nix run .#dev
  ```

- Run the server and the tests:

  ```bash
  cabal run unosteve
  cabal test                                       # database tests need `nix run .#dev`
  RUN_R2_TESTS=1 cabal test --test-options='--match "Storage.R2"'   # against the R2 dev bucket
  ```

- Format and check (what CI runs):

  ```bash
  nix fmt
  nix flake check -L
  ```

## License

BSD-3-Clause
