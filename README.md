# Riak PG

## Overview

Distributed process groups with riak\_core.  Work in progress,
not for production use.

## Usage

Join a process to a group (no need to pre-declare)

```erlang
join(term(), pid()) -> ok | {error, timeout}.
```

What about removing?

```erlang
leave(term(), pid()) -> ok | {error, timeout}.
```

You can delete process groups.

```erlang
delete(term()) -> ok | {error, timeout}.
```

You can also return the members, local members or connected members of a
group.

```erlang
members(term()) -> {ok, list(pid()) | {error, timeout}.
local_members(term()) -> {ok, list(pid()) | {error, timeout}.
connected_members(term()) -> {ok, list(pid()) | {error, timeout}.
```

You can list the groups that have been created and currently have members.

```erlang
groups() -> {ok, list(term())} | {error, timeout}
```

## Copyright

Copyright (C) 2013 Christopher Meiklejohn.
