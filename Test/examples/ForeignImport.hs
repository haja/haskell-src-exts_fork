foreign import ccall unsafe "getProgArgv"
 getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()