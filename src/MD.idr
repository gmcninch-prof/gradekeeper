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
  ListItem : ( header : List MDText) -> ( contents : List MDPar ) -> MDPar
  CodeBlock : ( lang : String ) -> ( contents : List String ) -> MDPar
  Section : ( header : List MDText ) -> ( contents : List MDPar) -> MDPar 
  Table : ( header : List MDText ) -> ( contents : List (List TableContents) ) -> MDPar

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

renderPar : Nat -> MDPar -> String        
renderPar pad (Normal xs) = multiIndent pad $ renderTextList xs
renderPar pad (Pre str) = multiIndent (pad + 2) $ str
renderPar pad (Quote xs) = multiIndent pad ("> " ++ renderTextList xs)
renderPar pad (ListItem header contents) = 
  joinBy "\n" $ the (List String) (hd::rest)
  where
    hd : String
    hd = indent pad ("- " ++ renderTextList header)
    
    rest : List String
    rest = renderPar (pad + 2) <$> contents

renderPar pad (CodeBlock lang contents) = 
  indent pad $ joinBy "\n" $ the (List String) ( [top] <+> contents <+> [bot] )
  where
    top : String
    top = "``` " ++ lang
    
    bot : String
    bot = "```"
renderPar pad (Section header contents) = 
  joinBy "\n" $ the (List String) (hd::rest)
  where
    hd : String
    hd = (replicate pad '#') ++ " " ++ renderTextList header
    
    rest : List String
    rest = renderPar (pad+1) <$> contents
    
renderPar pad (Table header contents) = ?renderPar_rhs_6
--   indent pad $ Fold.intercalate "\n" $ headline:headsep:Nil <>  (dataline <$> contents)
  where
    getWidths : List Nat -> List TableContents -> List Nat 
    getWidths old cl = let new = String.length <$> show <$> cl in
      (\(x,y) => max x y) <$> zip new old
  
    headerWidth : MDText -> Nat  
    headerWidth (Text str) = String.length str
    headerWidth (Emph str) = 2 + String.length str
    headerWidth (Bold str) = 4 + String.length str
    headerWidth (Struck xs) = 2 + (foldl (\i,t => i + headerWidth t) 0 xs)
    headerWidth (Code str) = 2 + String.length str
    headerWidth (Link (MkHRef target desc)) = String.length target + String.length desc + 4
    headerWidth (Image (MkImgRef target alt)) = String.length target + String.length alt + 4

    numColumns : Nat
    numColumns = List.length header

    widths : List Nat
    widths = foldl getWidths (headerWidth <$> header) contents
    
    vert : String
    vert = "|"
    
    hori : String
    hori = "-"


    headline = String.joinBy ""
             [ vert
             , " "
             , intercalate (" " ++ vert ++ " ") $ (\(Tuple w h) => SU.padEnd w $ renderText h) <$>
                (zip widths header)
             , " "
             , vert]


--   zeros :: Int -> List Int
--   zeros m | m>0 = 0:(zeros (m-1))
--   zeros _ = Nil


                  




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
  

