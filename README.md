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

## Copyright

Copyright (C) 2013 Christopher Meiklejohn.
