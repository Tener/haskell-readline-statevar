{-# LANGUAGE CPP #-} 
module System.Console.Readline.StateVar ( 
    readline ,
    addHistory ,
    
    {-
    getLineBuffer,        -- :: IO String
    setLineBuffer,        -- :: String -> IO ()
    -}
    lineBuffer,

    
    -- Functions involving point positions are meaningful only when string
    -- conversion between Haskell and C preserves the length.

    point, end, mark,
    {-
    getPoint,             -- :: IO Int
    setPoint,             -- :: Int -> IO ()
    getEnd,               -- :: IO Int
    setEnd,               -- :: Int -> IO ()
    getMark,              -- :: IO Int
    setMark,              -- :: Int -> IO ()
    -}
    
    done, pendingInput,
    {-
    setDone,              -- :: Bool -> IO ()
    setPendingInput,      -- :: Char -> IO ()
    -}

    eraseEmptyLine,
    {-
    setEraseEmptyLine,    -- :: Bool -> IO ()
    -}


    prompt,
    {-
    getPrompt,            -- :: IO String
    -}

    alreadyPrompted,
    {-
    setAlreadyPrompted,   -- :: Bool -> IO ()
    -}

    libraryVersion, terminalName, readLineName, inStream, outStream, startupHook,
    {-
    getLibraryVersion,    -- :: IO String
    getTerminalName,      -- :: IO String
    setReadlineName,      -- :: String -> IO ()
    getInStream,          -- :: IO Handle
    getOutStream,         -- :: IO Handle
    setStartupHook,       -- :: Maybe (IO ()) -> IO ()
    -}

    preInputHook,
    {-
    setPreInputHook,      -- :: Maybe (IO ()) -> IO ()
    -}

    eventHook, redisplayFunction,
    {-
    setEventHook,         -- :: Maybe (IO ()) -> IO ()
    setRedisplayFunction, -- :: Maybe (IO ()) -> IO ()
    -}
    
    
    -- | Keymaps
    Keymap,             -- data Keymap
    newBareKeymap ,
    copyKeymap ,
    newKeymap ,
    freeKeymap ,
    -- | Keymap vars
    keymap, keymapByName, keymapName, executingKeymap, bindingKeymap,
    {-
    getKeymap,          -- :: IO Keymap
    setKeymap,          -- :: Keymap -> IO ()
    getKeymapByName,    -- :: String -> IO Keymap
    getKeymapName,      -- :: Keymap -> IO (Maybe String)
    getExecutingKeymap, -- :: IO Keymap
    getBindingKeymap,   -- :: IO Keymap
    -}
    
    -- | Callbacks
    Callback,           -- type Callback = Int -> Char -> IO Int
    addDefun ,
    bindKey ,
    bindKeyInMap ,
    unbindKey ,
    unbindKeyInMap ,
    unbindCommandInMap ,
    Entry(..),          -- data Entry
                        --     = Function Callback
                        --     | Macro    String
                        --     | Keymap   Keymap
    genericBind ,
    parseAndBind ,
    readInitFile ,
    
    --------------------------------------------------------------------
    -- Associating Function Names and Bindings.
    
    namedFunction ,
    functionOfKeyseq ,
    functionDumper ,
    listFunmapNames ,

    funmapNames ,

    
    beginUndoGroup, endUndoGroup ,
    UndoCode(..),   -- data UndoCode
                    --     = UndoDelete
                    --     | UndoInsert
                    --     | UndoBegin
                    --     | UndoEnd
    addUndo ,
    freeUndoList ,
    doUndo ,
    modifying ,
    
    --------------------------------------------------------------------
    -- Redisplay.
    
    redisplay ,
    forcedUpdateDisplay ,
    onNewLine ,

    onNewLineWithPrompt ,

    resetLineState ,
    message ,
    clearMessage ,

    savePrompt ,
    restorePrompt ,

    --------------------------------------------------------------------
    -- Modifying Text.
    
    insertText ,
    deleteText ,
    copyText ,
    killText ,
    
    --------------------------------------------------------------------
    -- Utility functions.
    
    readKey ,
    stuffChar ,
    initialize ,
    resetTerminal ,
    ding ,

    displayMatchList ,
    
    --------------------------------------------------------------------
    -- Alternate Interface.
    
    callbackHandlerInstall ,
    callbackReadChar ,
        
    catchSignals, catchSigwinch, 
    
    {-
    setCatchSignals,    -- :: Bool -> IO ()
    getCatchSignals,    -- :: IO Bool
    setCatchSigwinch,   -- :: Bool -> IO ()
    getCatchSigwinch,   -- :: IO Bool
    -}
    cleanupAfterSignal ,
    freeLineState ,
    resetAfterSignal ,
    resizeTerminal ,

    setSignals ,
    clearSignals ,
    
    --------------------------------------------------------------------
    -- Completion functions.
    
    completeInternal ,
    complete ,
    possibleCompletions ,
    insertCompletions ,
    -- readline uses functions that are called multiple times and
    -- return an entry at a time, maintaining their state at which
    -- point they are. This is silly in a functional language so here
    -- we work with functions String -> IO [String].
    completionMatches ,
    filenameCompletionFunction ,
    usernameCompletionFunction ,
    
    completionEntryFunction, attemptedCompletionFunction, filenameQuotingFunction,
{-
    setCompletionEntryFunction,
        -- :: Maybe (String -> IO [String]) -> IO ()
    setAttemptedCompletionFunction,
        -- :: Maybe (String -> Int -> Int -> IO (Maybe (String, [String]))) -> IO ()
    setFilenameQuotingFunction,
        -- :: Maybe (String -> Bool -> Ptr CChar -> IO String) -> IO ()
-}
    quoteFilename ,
    filenameDequotingFunction, charIsQuotedP, completionQueryItems,
{-
    setFilenameDequotingFunction,
        -- :: Maybe (String -> Maybe Char -> IO String) -> IO ()
    setCharIsQuotedP,
        -- :: Maybe (String -> Int -> IO Bool) -> IO ()
    getCompletionQueryItems,          -- :: IO Int
    setCompletionQueryItems,          -- :: Int -> IO ()
-}

    basicWordBreakCharacters,      
    basicQuoteCharacters,          
    completerWordBreakCharacters,  
    completerQuoteCharacters,      
    filenameQuoteCharacters,       
    specialPrefixes,               
    completionAppendCharacter,     
    ignoreCompletionDuplicates,    
    filenameCompletionDesired,     
    filenameQuotingDesired,        
    inhibitCompletion,             
    attemptedCompletionOver,       
    ignoreSomeCompletionsFunction, 
    directoryCompletionHook,
    completionWordBreakHook,
    completionDisplayMatchesHook

{-
    getBasicWordBreakCharacters,      -- :: IO String
    setBasicWordBreakCharacters,      -- :: String -> IO ()
    getBasicQuoteCharacters,          -- :: IO String
    setBasicQuoteCharacters,          -- :: String -> IO ()
    getCompleterWordBreakCharacters,  -- :: IO String
    setCompleterWordBreakCharacters,  -- :: String -> IO ()
    getCompleterQuoteCharacters,      -- :: IO String
    setCompleterQuoteCharacters,      -- :: String -> IO ()
    getFilenameQuoteCharacters,       -- :: IO String
    setFilenameQuoteCharacters,       -- :: String -> IO ()
    getSpecialPrefixes,               -- :: IO String
    setSpecialPrefixes,               -- :: String -> IO ()
    getCompletionAppendCharacter,     -- :: IO (Maybe Char)
    setCompletionAppendCharacter,     -- :: Maybe Char -> IO ()
    setIgnoreCompletionDuplicates,    -- :: Bool -> IO ()
    getIgnoreCompletionDuplicates,    -- :: IO Bool
    setFilenameCompletionDesired,     -- :: Bool -> IO ()
    getFilenameCompletionDesired,     -- :: IO Bool
    setFilenameQuotingDesired,        -- :: Bool -> IO ()
    getFilenameQuotingDesired,        -- :: IO Bool
    setInhibitCompletion,             -- :: Bool -> IO ()
    getInhibitCompletion,             -- :: IO Bool
    setAttemptedCompletionOver,       -- :: Bool -> IO ()
    getAttemptedCompletionOver,       -- :: IO Bool
    setIgnoreSomeCompletionsFunction,
        -- :: Maybe ([String] -> IO [String]) -> IO ()
        -- The function may not make the list longer!
    setDirectoryCompletionHook
        -- :: Maybe (String -> IO String) -> IO ()
    setCompletionWordBreakHook
    setCompletionDisplayMatchesHook
-}

    )
where

import System.Console.Readline
import Data.StateVar
import System.IO
import Foreign
import Foreign.C



lineBuffer = makeStateVar getLineBuffer setLineBuffer
point      = makeStateVar getPoint      setPoint
end        = makeStateVar getEnd        setEnd
mark       = makeStateVar getMark       setMark
keymap     = makeStateVar getKeymap     setKeymap

done            = makeSettableStateVar setDone
pendingInput    = makeSettableStateVar setPendingInput
eraseEmptyLine  = makeSettableStateVar setEraseEmptyLine
alreadyPrompted = makeSettableStateVar setAlreadyPrompted
readLineName    = makeSettableStateVar setReadlineName

startupHook       = makeSettableStateVar setStartupHook
preInputHook      = makeSettableStateVar setPreInputHook
eventHook         = makeSettableStateVar setEventHook
redisplayFunction = makeSettableStateVar setRedisplayFunction


prompt         = makeGettableStateVar getPrompt
libraryVersion = makeGettableStateVar getLibraryVersion
terminalName   = makeGettableStateVar getTerminalName

inStream        = makeGettableStateVar getInStream
outStream       = makeGettableStateVar getOutStream
keymapByName nm = makeGettableStateVar (getKeymapByName nm)
keymapName km   = makeGettableStateVar (getKeymapName km)

executingKeymap = makeGettableStateVar getExecutingKeymap
bindingKeymap   = makeGettableStateVar getBindingKeymap 

---

catchSignals  = makeStateVar getCatchSignals  setCatchSignals
catchSigwinch = makeStateVar getCatchSigwinch setCatchSigwinch

completionEntryFunction     = makeSettableStateVar setCompletionEntryFunction
attemptedCompletionFunction = makeSettableStateVar setAttemptedCompletionFunction
filenameQuotingFunction     = makeSettableStateVar setFilenameQuotingFunction
filenameDequotingFunction   = makeSettableStateVar setFilenameDequotingFunction
charIsQuotedP               = makeSettableStateVar setCharIsQuotedP

completionQueryItems         = makeStateVar getCompletionQueryItems         setCompletionQueryItems 
basicWordBreakCharacters     = makeStateVar getBasicWordBreakCharacters     setBasicWordBreakCharacters 
basicQuoteCharacters         = makeStateVar getBasicQuoteCharacters         setBasicQuoteCharacters 
completerWordBreakCharacters = makeStateVar getCompleterWordBreakCharacters setCompleterWordBreakCharacters
completerQuoteCharacters     = makeStateVar getCompleterQuoteCharacters     setCompleterQuoteCharacters 
filenameQuoteCharacters      = makeStateVar getFilenameQuoteCharacters      setFilenameQuoteCharacters 
specialPrefixes              = makeStateVar getSpecialPrefixes              setSpecialPrefixes 
completionAppendCharacter    = makeStateVar getCompletionAppendCharacter    setCompletionAppendCharacter 
ignoreCompletionDuplicates   = makeStateVar getIgnoreCompletionDuplicates   setIgnoreCompletionDuplicates
filenameCompletionDesired    = makeStateVar getFilenameCompletionDesired    setFilenameCompletionDesired
filenameQuotingDesired       = makeStateVar getFilenameQuotingDesired       setFilenameQuotingDesired 
inhibitCompletion            = makeStateVar getInhibitCompletion            setInhibitCompletion 
attemptedCompletionOver      = makeStateVar getAttemptedCompletionOver      setAttemptedCompletionOver

ignoreSomeCompletionsFunction = makeSettableStateVar setIgnoreSomeCompletionsFunction
directoryCompletionHook       = makeSettableStateVar setDirectoryCompletionHook
completionWordBreakHook       = makeSettableStateVar setCompletionWordBreakHook
completionDisplayMatchesHook  = makeSettableStateVar setCompletionDisplayMatchesHook

--

alreadyPrompted :: SettableStateVar Bool
attemptedCompletionFunction :: SettableStateVar
                               (Maybe (String -> Int -> Int -> IO (Maybe (String, [String]))))
attemptedCompletionOver :: StateVar Bool
basicQuoteCharacters :: StateVar String
basicWordBreakCharacters :: StateVar String
bindingKeymap :: GettableStateVar Keymap
catchSignals :: StateVar Bool
catchSigwinch :: StateVar Bool
charIsQuotedP :: SettableStateVar
                   (Maybe (String -> Int -> IO Bool))
completerQuoteCharacters :: StateVar String
completerWordBreakCharacters :: StateVar String
completionAppendCharacter :: StateVar (Maybe Char)
completionDisplayMatchesHook :: SettableStateVar
                                  (Maybe ([String] -> IO ()))
completionEntryFunction :: SettableStateVar
                             (Maybe (String -> IO [String]))
completionQueryItems :: StateVar Int
completionWordBreakHook :: SettableStateVar
                             (Maybe (IO (Maybe String)))
directoryCompletionHook :: SettableStateVar
                             (Maybe (String -> IO String))
done :: SettableStateVar Bool
end :: StateVar Int
eraseEmptyLine :: SettableStateVar Bool
eventHook :: SettableStateVar (Maybe (IO ()))
executingKeymap :: GettableStateVar Keymap
filenameCompletionDesired :: StateVar Bool
filenameDequotingFunction :: SettableStateVar
                               (Maybe (String -> Maybe Char -> IO String))
filenameQuoteCharacters :: StateVar String
filenameQuotingDesired :: StateVar Bool
filenameQuotingFunction :: SettableStateVar
                             (Maybe
                                (String
                                 -> Bool
                                 -> Ptr CChar
                                 -> IO String))
ignoreCompletionDuplicates :: StateVar Bool
ignoreSomeCompletionsFunction :: SettableStateVar
                                   (Maybe ([String] -> IO [String]))
inStream :: GettableStateVar Handle
inhibitCompletion :: StateVar Bool
keymap :: StateVar Keymap
keymapByName :: String -> GettableStateVar Keymap
keymapName :: Keymap -> GettableStateVar (Maybe String)
libraryVersion :: GettableStateVar String
lineBuffer :: StateVar String
mark :: StateVar Int
outStream :: GettableStateVar Handle
pendingInput :: SettableStateVar Char
point :: StateVar Int
preInputHook :: SettableStateVar (Maybe (IO ()))
prompt :: GettableStateVar String
readLineName :: SettableStateVar String
redisplayFunction :: SettableStateVar (Maybe (IO ()))
specialPrefixes :: StateVar String
startupHook :: SettableStateVar (Maybe (IO ()))
terminalName :: GettableStateVar String

