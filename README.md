
# TASKFLOW

A simple haskell implementation of a to do list task manager.

# Building

Build the Project
To build the project, run:

cabal build
This will compile the Haskell source code using Cabal.

# Command-Line Usage
The application is used via the command line with the following structure:

todo <list-name> <command> [arguments...]

   | Command  | Description                                      |
| -------- | ------------------------------------------------ |
| `create` | Create a new to-do list with the specified name. |
| `list`   | Display all tasks in the specified to-do list.   |
| `add`    | Add one or more tasks to the list.               |
| `delete` | Remove a task by its ID from the list.           |

Just specify the name of the todo list you would like to interaction with, and state what you would like to to do with it.

### Create A New List

$ todo house create
   
### Add Tasks to the List

    $ todo house add "Walk the dog."
    
    $ todo house add "Take out the mail." 
                     "Pick up the trash."

### or to add multiple at once:

    $ todo house add "Walk the dog." 
                     "Take out the mail." 
                     "Pick up the trash."

### View All Tasks

$ todo house list

1 - Walk the dog.
2 - Take out the mail.
3 - Pick up the trash.

### Delete Incorrect Tasks
Deleted task 2 from the 'house' list.

Deleted task 3 from the 'house' list.


### Add Corrected Tasks
$ todo house list

1 - Walk the dog.
2 - Take out the trash.
3 - Pick up the mail.

### Create Another List (e.g., Groceries)

    $ todo house delete 2
    $ todo house delete 3
    $ todo house add "Take out the trash." "Pick up the mail."
    
## Features

Support for multiple independent to-do lists
Add, view, and remove tasks easily via the terminal
Simple and clean user interface
Built entirely in Haskell, leveraging functional programming principles

## License

This project is open-source and released under the MIT License.


