module LogAnalysis where
import Log
import Data.Typeable

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

parseMessage :: String -> LogMessage
parseMessage = parseStrings.words

parseStrings :: [String] -> LogMessage
parseStrings s@("I":ts:ss) = 
	if(isInt ts) 
		then LogMessage Info (read ts) (unwords ss)
		else Unknown (unwords s)
parseStrings s@("W":ts:ss) = 
	if(isInt ts)
		then LogMessage Warning (read ts) (unwords ss) 
		else Unknown (unwords s)
parseStrings s@("E":level:ts:ss) = 
	if(isInt ts && isInt level)
		then LogMessage (Error (read level)) (read ts) (unwords ss)
		else Unknown (unwords s)
parseStrings ss = Unknown (unwords ss)

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = if(x>='0' && x<='9') 
	then isInt xs
	else False  

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node l m r) = 
	if((getTS msg)< (getTS m)) 
		then Node (insert msg l) m r
		else Node l m (insert msg r)

getTS :: LogMessage -> Int
getTS (Unknown _) = 0
getTS (LogMessage _ ts _) = ts

build :: [LogMessage]-> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extract.inOrder.build

extract :: [LogMessage] -> [String]
extract [] = []
extract ((x@(LogMessage (Error v) _ s)):xs) = 
	if(v>=50) 
		then s:(extract xs)
		else extract xs
extract (x:xs) = extract xs