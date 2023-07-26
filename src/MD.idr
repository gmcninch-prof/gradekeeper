module MD
import Derive.Prelude
import Data.Vect

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

public export 
data MDText : Type where
  Text : String -> MDText
  Emph : String -> MDText
  Bold : String -> MDText
  Struck : List MDText -> MDText
  Code : String -> MDText
  Link : HRef -> MDText
  Image : ImgRef -> MDText

public export 
data TableContents : Type where
  CString : String -> TableContents
  CInt :  Int -> TableContents
  CDouble : Double -> TableContents

%runElab derive "TableContents" [ Show, Eq ]

renderTC : TableContents -> String
renderTC (CString str) = str
renderTC (CInt i) = show i
renderTC (CDouble dbl) = show dbl

public export 
data MDPar : Type where
  Normal : List MDText -> MDPar
  Pre : String -> MDPar
  Quote : List MDText -> MDPar
  ListItem : ( header : List MDText) -> ( contents : List MDPar ) -> MDPar
  CodeBlock : ( lang : String ) -> ( contents : List String ) -> MDPar
  Section : ( header : List MDText ) -> ( contents : List MDPar) -> MDPar 
  Table : { ncols : Nat} -> ( header : Vect ncols MDText ) -> ( contents : List (Vect ncols TableContents) ) -> MDPar

public export 
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

repeat : Nat -> String -> String
repeat 0 str = ""
repeat (S k) str = str <+> repeat k str


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
  multiIndent pad $ joinBy "\n" $ the (List String) ( [top] <+> contents <+> [bot] )
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
    
renderPar pad (Table {ncols} header contents) = 
  multiIndent pad $ String.joinBy "\n" $ headline::headsep::(dataline <$> contents)
  where
    getWidths : Vect ncols Nat -> Vect ncols TableContents -> Vect ncols Nat 
    getWidths old cl = let new = String.length <$> renderTC <$> cl in
      (\(x,y) => max x y) <$> zip new old
  
    headerWidth : MDText -> Nat  
    headerWidth (Text str) = String.length str
    headerWidth (Emph str) = 2 + String.length str
    headerWidth (Bold str) = 4 + String.length str
    headerWidth (Struck xs) = 2 + (foldl (\i,t => i + headerWidth t) 0 xs)
    headerWidth (Code str) = 2 + String.length str
    headerWidth (Link (MkHRef target desc)) = String.length target + String.length desc + 4
    headerWidth (Image (MkImgRef target alt)) = String.length target + String.length alt + 4

    -- numColumns : Nat
    -- numColumns = List.length header

    widths : Vect ncols Nat
    widths = foldl getWidths (headerWidth <$> header) contents
    
    vert : String
    vert = "|"
    
    hori : String
    hori = "-"

    hdrStrContents : String
    hdrStrContents = joinBy (" " ++ vert ++ " ")
                     $ toList $ (\(w, h) => String.padRight w ' ' (renderText h)) <$> (zip widths header)


    headline : String
    headline = String.joinBy "" $ [ vert , " ", hdrStrContents, " " , vert]


    headsep : String
    headsep = String.joinBy ""
              [ vert
              , hori
              , String.joinBy (hori <+> vert <+> hori) $ toList $ (\w => repeat w hori) <$> widths
              , hori
              , vert
              ]


    dataline : Vect ncols TableContents -> String
    dataline ltc =
      String.joinBy ""
      [ vert
      , " "
      , String.joinBy (" " ++ vert ++ " ") $ toList $ (\(w, s) => String.padRight w ' ' $ renderTC s) <$> (zip widths ltc)
      , " "
      , vert
      ]


renderPars : List MDPar -> String
renderPars [] = ""
renderPars (x :: xs) = joinBy "\n" $ (renderPar 1 x) :: (renderPars xs) :: Nil

            

public export 
render : MD -> String
render (MkMD date title author content) = 
  meta <+> renderPars content
  where
    meta : String
    meta = String.joinBy "\n" $
      catMaybes [ Just "---"
                , (\a => "author: " ++ a) <$>  author
                , (\t => "title: " ++ t) <$> title
                , (\d => "date: " ++ d) <$> date
                , Just "header-includes:  \\usepackage{palatino,mathpazo}"
                , Just "---\n\n"
                ]
  

