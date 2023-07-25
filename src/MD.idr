module MD
import Derive.Prelude

%language ElabReflection

record HRef where
  constructor MkHRef
  target : String
  desc : String

%runElab derive "HRef" [ Show, Eq ]

record ImgRef where
  constructor MkImgRef
  target : String
  alt : String

%runElab derive "ImgRef" [ Show, Eq ]

data MDText : Type where
  Text : String -> MDText
  Emph : String -> MDText
  Bold : String -> MDText
  Struck : List MDText -> MDText
  Code : String -> MDText
  Link : HRef -> MDText
  Image : ImgRef -> MDText

data TableContents : Type where
  CString : String -> TableContents
  CInt :  Int -> TableContents
  CDouble : Double -> TableContents

%runElab derive "TableContents" [ Show, Eq ]

data MDPar : Type where
  Normal : List MDText -> MDPar
  Pre : String -> MDPar
  Quote : List MDText -> MDPar
  ListItem : { header : List MDText} -> { contents : List MDPar } -> MDPar
  CodeBlock : { lang : String } -> { contents : List MDPar } -> MDPar
  Section : { header : List MDText } -> { contents : List MDPar} -> MDPar 
  Table : { header : List MDText } -> { contents : List (List TableContents) } -> MDPar

record MD where
  constructor MkMD
  date : Maybe String
  title : Maybe String
  author : Maybe String
  content : List MDPar
  

multiIndent : Nat -> String -> String
multiIndent k str = joinBy "\n" $ List1.forget pls
  where
    ls : List1 String
    ls = String.split (\c => c == '\n') str

    pls : List1 String
    pls = map (indent k) ls  

renderText : MDText -> String
renderText (Text str) = str
renderText (Emph str) = "*" ++ str ++ "*"
renderText (Bold str) = "**" ++ str ++ "**"
renderText (Struck xs) = "~" ++ (joinBy " " (renderText <$> xs)) ++ "~"
renderText (Code str) = "`" ++ str ++ "`"
renderText (Link (MkHRef target desc)) = "[" ++ desc ++ "](" ++ target ++ ")"
renderText (Image (MkImgRef target alt)) = "![" ++ alt ++ "](" ++ target ++ ")"

renderTextList : List MDText -> String
renderTextList lt = joinBy " " $ renderText <$> lt

renderPar : Nat -> Nat -> MDPar -> String        
renderPar _ pad (Normal xs) = multiIndent pad $ renderTextList xs
renderPar _ pad (Pre str) = multiIndent (pad + 2) $ str
renderPar _ pad (Quote xs) = ?renderPar_rhs_2
renderPar lvl pad ListItem = ?renderPar_rhs_3
renderPar lvl pad CodeBlock = ?renderPar_rhs_4
renderPar lvl pad Section = ?renderPar_rhs_5
renderPar lvl pad Table = ?renderPar_rhs_6


-- renderPar _ pad (Quote xt) =
--   pad
--     <> "> "
--     <> renderTextList xt

-- renderPar n pad (ListItem { header, contents }) =
--   joinWith "\n"
--     $ (:)
--         (pad <> "- " <> renderTextList header)
--         (renderPar n (pad <> "  ") <$> contents)

-- renderPar _ pad (CodeBlock { lang, contents }) =
--   indent pad
--     $ joinWith "\n"
--     $ ("``` " <> lang)
--     : contents
--     <> "```"
--     : Nil

-- renderPar n _ (Section { header, contents }) =
--   joinWith "\n"
--     $ (:)
--         ((repeat n "#") <> " " <> renderTextList header)
--         (renderPar (n + 1) (repeat (n + 1) " ") <$> contents)

-- renderPar _ pad (Table { header, contents }) =
  
--   indent pad $ Fold.intercalate "\n" $ headline:headsep:Nil <>  (dataline <$> contents)
--   where
--   getWidths :: List Int -> List TableContents -> List Int
--   getWidths old cl =
--     let new = String.length <$> display <$> cl in
--       (\(Tuple x y) -> max x y) <$> zip new old
    

--   headerWidth :: Text -> Int
--   headerWidth (Text a)  = String.length a
--   headerWidth (Bold a) = 4 + String.length a
--   headerWidth (Emph a) = 2 + String.length a
--   headerWidth (Struck a) = 2 + (Fold.foldl (\i t -> i+headerWidth t) 0 a)
--   headerWidth (Code a) = 2 + String.length a
--   headerWidth (Link href) = String.length href.target + String.length href.desc + 4
--   headerWidth (Image href) = String.length href.target + String.length href.alt + 4  

--   zeros :: Int -> List Int
--   zeros m | m>0 = 0:(zeros (m-1))
--   zeros _ = Nil

--   numColumns = length header
                  
--   widths = Fold.foldl getWidths (headerWidth <$> header) contents

--   vert = "|"
--   hori = "-"

--   headline = String.joinWith ""
--              [ vert
--              , " "
--              , Fold.intercalate (" " <> vert <> " ") $ (\(Tuple w h) -> SU.padEnd w $ renderText h) <$>
--                 (zip widths header)
--              , " "
--              , vert]

--   headsep = String.joinWith ""
--             [ vert
--             , hori
--             , Fold.intercalate (hori <> vert <> hori) $ (\w -> fromMaybe "" $ SU.repeat w hori) <$>
--                widths
--             , hori
--             , vert]


--   dataline :: List TableContents -> String
--   dataline ltc =
--     String.joinWith ""
--     [ vert
--     , " "
--     , Fold.intercalate (" " <> vert <> " ") $ (\(Tuple w s) -> SU.padEnd w $ display s) <$> (zip widths ltc)
--     , " "
--     , vert
--     ]

      
                  
-- text : String -> MDText
-- text = Text

-- emph : String -> MDText
-- emph = Emph

-- bold : String -> MDText
-- bold = Bold

-- struck : List MDText -> MDText
-- struck = Struck

-- code : String -> MDText
-- code = Code

-- link : HRef -> MDText
-- link = Link

-- img : ImgRef -> MDText
-- img = Image

-- normal : List MDText -> MDPar
-- normal = Normal

-- pre : String -> MDPar
-- pre = Pre

-- quote : List MDText -> MDPar
-- quote = Quote

-- li : { header : List MDText } ->  {contents : List MDPar } -> MDPar
-- li = ListItem

-- codeblock :: { lang :: String, contents :: List String } -> MDPar
-- codeblock = CodeBlock

-- section :: { header :: List Text, contents :: List MDPar } -> MDPar
-- section = Section

-- joinWith :: String -> List String -> String
-- joinWith _ Nil = ""

-- joinWith s (x : xs) = x <> s <> (joinWith s xs)

-- repeat :: Int -> String -> String
-- repeat n s
--   | n == 1 = s

-- repeat n s
--   | n > 1 = s <> repeat (n - 1) s

-- repeat _ _ = ""


-- -- --------------------------------------------------------------------------------


-- renderPars :: List MDPar -> String
-- renderPars Nil = mempty
-- renderPars (x : xs) = joinWith "\n" $ (renderPar 1 "" x) : (renderPars xs) : Nil

-- render :: MD -> String
-- render { author, date, title, content } =
--   meta <> renderPars content
--   where
--     meta =
--       Fold.intercalate "\n" $
--       catMaybes $ (Just "---")
--       : ((\a -> "author: " <> a) <$>  author)
--       : ((\t -> "title: " <> t) <$> title)
--       : ((\d -> "date: " <> d) <$> date)
--       : Just "header-includes:  \\usepackage{palatino,mathpazo}"
--       : Just "---\n\n"
--       : Nil
  

