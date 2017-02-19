Some helpers that make livecoding at little easier with:

- slime/swank
- sly/slynk

Exposes:

# #'update-repl-link

Call this function to get the server to handle requests. Put this in your main game/demo/etc loop to keep the repl live whist your code runs.

# #'peek

A quick way of calling inspecting an object in your editor

# continuable

A macro commonly used in livecoding to enable continuing when errors are raised. Simply wrap around a chunk of code and it provides a restart called 'continue' which ignores the error and carrys on from the end of the body. I usually wrap this around the code 'step' function in my main loop.
