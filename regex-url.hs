import Text.Regex.Applicative

data Protocol = HTTP | FTP deriving Show

protocol :: RE Char Protocol

protocol = HTTP <$ string "http" <|> FTP <$ string "ftp"

type Host = String

type Location = String

data URL = URL Protocol Host Location deriving Show

host :: RE Char Host

host = many $ psym $ (/= '/')

url :: RE Char URL

url = URL <$> protocol <* string "://" <*> host <* sym '/' <*> many anySym

main = print $ "http://stackoverflow.com/questions" =~ url
