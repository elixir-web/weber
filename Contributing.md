Contributing
================

This document describes the usages and rules to follow when contributing
to [Weber](https://github.com/0xAX/weber).

Bugs, Issues, Suggestions
==========================

If you will have any questsions, suggestions or bug repports, you should
create a ticket in [issues](https://github.com/0xAX/weber/issues).

If you will ideas about new `Weber's` feature, you should create a ticket
in [issues](https://github.com/0xAX/weber/issues) with `[PROPOSAL]` theme,
see [example](https://github.com/0xAX/weber/issues/28) for further discussion.

Style guide
===================

If you will send patch to the Weber your code should follow some simple rules:

  * We use 2 spaces for Elixir code.
  * Variables example: `file_name`, `file_content`.

Developing
====================

First of all you must fork the project's repository to your GitHub account by 
clicking on the `Fork` button. Then you must clone your fork to the local machine
with:

```
git clone https://github.com/your_nick/weber.git
```

and add official remote repo  url:

```
git remote add upstream https://github.com/0xAX/weber.git
```

Before starting working on the code, you MUST update to `upstream`.
You should make sure that you start making changes to the current version
of the Weber.

For updating the current branch to `upstream`:

```
git fetch upstream
git rebase upstream/master
```

You MUST create a new branch for your work. First, ensure you are on `master`,
and execute:

```
git checkout -b "feature-1"
```

make your changes and push it:

```
git push origin feature-1
```

Go to the Github and send Pull Request.

Testing
==============================

If  you made new non-trivial feature or bug fix, please write nit test for it. 

There are 2 types of tests in Weber:

  1. You can test usual function as we doing it in [route_test.ex](https://github.com/0xAX/weber/blob/master/test/route_test.exs)
  
  2. If you need to test Weber in working mode (you need to test request/response and etc...) we use hackney web client and exunit too. See [response_test](https://github.com/0xAX/weber/blob/master/test/weberTest/response_test.exs)

Running tests with:

```
mix test --no-start
```

or

```
make test
```
