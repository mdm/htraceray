module Config where

import Options.Applicative

data Config = Config
    { width :: Int
    , height :: Int
    , samples :: Int
    , inputFile :: String
    , outputFile :: String
    , chunkSize :: Int
    }

config :: Parser Config
config = Config
         <$> option auto
             ( long "width"
            <> short 'w' 
            <> help "Width of the output image in pixels"
            <> showDefault
            <> value 200
            <> metavar "<WIDTH>"
             )
         <*> option auto
             ( long "height"
            <> short 'h' 
            <> help "Height of the output image in pixels"
            <> showDefault
            <> value 100
            <> metavar "<HEIGHT>"
             )
         <*> option auto
             ( long "samples"
            <> short 's' 
            <> help "Number of rays per pixel"
            <> showDefault
            <> value 10
            <> metavar "<SAMPLES>"
             )
         <*> strOption
             ( long "input"
            <> short 'i' 
            <> help "JSON file containing a scene description"
            <> metavar "<INPUT_FILE>"
             )
         <*> strOption
             ( long "output"
            <> short 'o' 
            <> help "PNG file to write the output to"
            <> metavar "<OUTPUT_FILE>"
             )
         <*> option auto
             ( long "chunk"
            <> short 'c' 
            <> help "Chunk size for multi-core processing"
            <> showDefault
            <> value 500
            <> metavar "<SIZE>"
             )

options :: ParserInfo Config
options = info (config <**> helper)
          ( fullDesc
         <> progDesc "A simple ray-tracer based on \"Ray Tracing in One Weekend\""
         <> header "htraceray - Ray-Tracing in Haskell" )