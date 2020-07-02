In Haskell, you can access the command line arguments with `getArgs`. 

We have to import `System.Environment` to get access to the `getArgs` function. 

The environment library also comes with some useful functions like `getEnv` and `setEnv` for using environment variables.

```sh
import System.Environment

main =
    do    
        args <- getArgs
```

- The `head` function pulls the first element from a list. The variable args is a list of all the commnad line arguments provided to the program at run time.