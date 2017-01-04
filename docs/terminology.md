# groups, processes, roles, authorization

## group

*Groups* are the entities that represent organisations ("liqd.net"),
sub-organisations ("liqd developers", "liqd board"), or special
interests ("nuke the whales", "equal rights for part-time employees").

Users can CRUD (Create/Read/Update/Delete) groups.  A user who creates
a group has the *initiator* role in that group (see "roles" below).

Groups form a directed acyclic graph (DAG): Every group can have one
or more children, but other than in a tree-shaped hierarchy, every
child can have multiple parents.

```haskell
data Group = Group (UID Group) MetaInfo Title Description Parents Children
type Parents = [UID Group]
type Children = [UID Group]

data MetaInfo = MetaInfo Created CreatedAt ...
type Created = UserHandle
type UserHandle = ST
type CreateAt = Timestamp
```

## process

A process has a certain type (more general: "collecting wild ideas",
"collaborative text editing", ...; or more specific: "spending a
school budget", "updating a party constitution", "setting up a Ltd.",
...) that determines much of what it looks like and how it works.

Like groups, processes are initiated by users.  Unlike groups, a
process always has exactly one parent group ("its *home group*", or
just "its group") and no children.

```haskell
data Process = Process (UID Process) MetaInfo Title Description (UID Group)
```


## inviting users

(See "roles and permissions" below for a more complete picture of this
section.)

The initiator of a group or a process can invite users to it.  Sets of
users can be given in one of the following forms:

```haskell
data Members = Map (UID (Group | Process)) [UserSet]
data UserSet =
    CopyGroup (UID Group)    -- look up all group members now and invite them
  | LinkGroup (UID Group)    -- at the time of checking auth, lookup group members
  | UserHandle UserHandle    -- explicit list of individual users
  | Authenticated            -- everybody logged in with an account on the system
  | Anonymous                -- everybody, whether logged in or not
```

`LinkGroups` requires a tighter trust relationship between the groups:
it means that the linked group can invite members to your group at any
time, without you noticing (unless you have set the relevant
notifications); on the other hand (and this is true for both
'CopyGroup' and 'LinkGroup'), the linking group gets information about
the members of the linked group (at least partially, from the moment
on members of the linked group leave traces in the linking group, like
non-anonymous comments).


## roles and permissions

The process type determines the list of assignable roles (essentially
just badges the user can present to gain certain permissions) and what
permissions the user gains by carrying what role.  This part can not
be changed except between releases.

The initiator determines which roles are assigned to which users.
This part can be changed any time on the UI.

Preliminary list of roles for our first process type(s):

- (no role: can do nothing, not even see the existence of a process in
  a directory)
- ReadOnly
- Member
- Moderator
- LocalAdmin  -- difference between LocalAdmin and ProcessInitiator?
- ProcessInitiator
- GroupInitiator


    Student    { _roleSchoolClass :: SchoolClass }
  | ClassGuest { _roleSchoolClass :: SchoolClass } -- ^ e.g., parents
  | SchoolGuest  -- ^ e.g., researchers
  | Moderator
  | Principal
  | Admin

Roles that will probably become requirements at some point, but only
for certain process types:

- Delegate
- Board member
- Consultant (can make proposals like Member, but can not vote)
- ...

Now we can refine what we said in the last section: Membership is not
defined on a per-group (or per-process) basis, like this:

```haskell
data Members = Map (UID (Group | Process)) [UserSet]
```

...  but on a per-group--and-role basis, like this:

```haskell
data Members = Map (UID (Group | Process), Role) [UserSet]
```


## process directories

Every group has a directory of its home processes, but the group
initiators / moderators can also link to other proceses or groups
(with the listee's permission).


## open problems

FIXME: how do we model the close similarity between group and process?

FIXME: how do we model that every group and every process has its own,
individual set of roles?  data families?
