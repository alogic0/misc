import System.Process
import System.Posix.IO
import System.Posix.Types
import Data.IORef
import System.IO.Unsafe

{-# NOINLINE pid_ref #-}
pid_ref :: IORef (ProcessHandle, Fd)
pid_ref = unsafePerformIO $ newIORef undefined

browseLess :: String -> IO String
browseLess mod_ = do
  -- make a copy of stdout
  stdout_copy <- dup stdOutput

  -- launch less that reads from a pipe and writes to the terminal
  (Just pipe_handle, Nothing, Nothing, pid)
    <- createProcess (proc "less" []) { std_in = CreatePipe }

  closeFd stdOutput

  -- obtain an fd for our end of the pipe
  pipe_fd <- handleToFd pipe_handle

  -- make the pipe our new stdout
  dupTo pipe_fd stdOutput

  -- close pipe_fd, so that less can observe the EOF
  closeFd pipe_fd

  writeIORef pid_ref (pid, stdout_copy)

  return $ unlines
    [ ":browse " ++ mod_
    , "finish"
    ]

finish = do
  closeFd stdOutput
  (pid, stdout_copy) <- readIORef pid_ref
  waitForProcess pid
  dupTo stdout_copy stdOutput
  closeFd stdout_copy
