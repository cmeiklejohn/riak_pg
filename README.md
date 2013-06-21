# Riak PG

## Overview

Distributed process groups with riak\_core.  Work in progress,
not for production use.

## Usage

First, you can create process groups.

```erlang
create(term()) -> ok | {error, timeout}.
```

Or, you can delete process groups.

```erlang
delete(term()) -> ok | {error, timeout}.
```

These are not required, as you can just add a process to a group before
it exists.  They are mainly provided for compatibility with pg2.

Now, how do I add process to groups?

```erlang
join(term(), pid()) -> ok | {error, timeout}.
```

What about removing?

```erlang
leave(term(), pid()) -> ok | {error, timeout}.
```

You can also return the members, local members or connected members of a
group.

```erlang
members(term()) -> {ok, []} | {error, timeout}.
local_members(term()) -> {ok, []} | {error, timeout}.
connected_members(term()) -> {ok, []} | {error, timeout}.
```

## Copyright

Copyright (C) 2013 Christopher Meiklejohn.
