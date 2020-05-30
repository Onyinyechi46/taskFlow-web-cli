
# Haskell Todo List

A simple haskell implementation of a Todo List for managing tasks.

# Building

This project uses the Cabal tools for building the Haskell code. To build, simply do

    cabal build

once you have Cabal installed.

# Usage

Usage is fairly straight forward:

    Usage: todo LIST OPTION [ARGUMENT]...

    Options:

    - list:     List tasks in todo list.
    - create:   Create a new todo list.
    - add:      Add a new task to the specified todo list.
    - delete:   Delete task with the specified ID from the todo list.

Just specify the name of the todo list you would like to interaction with, and state what you would like to to do with it.

## Examples

Lets say you wanted to create a todo list for things you need to do around the house:

    $ todo house create

You need to wash the dog, take out the trash, and pick up the mail.

    $ todo house add "Walk the dog."
    $ todo house add "Take out the mail."
    $ todo house add "Pick up the trash."

or to add multiple at once:

    $ todo house add "Walk the dog." "Take out the mail." "Pick up the trash."

Okay let's look at our list:

    $ todo house list

    1 - Walk the dog.
    2 - Take out the mail.
    3 - Pick up the trash.

Whoops! Looks like we made a mistake entering those last two in.
This doesn't pose a problem, we can just remove them, and add them back:

    $ todo house delete 2
    $ todo house delete 3
    $ todo house add "Take out the trash." "Pick up the mail."

And now:

    $ todo house list

    1 - Walk the dog.
    2 - Take out the trash.
    3 - Pick up the mail.

Nice!

Now say you want to create a new list for groceries you need to pick up at the store:

    $ todo groceries create

Pretty great, right?