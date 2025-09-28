{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data MenuOption = Play | Info | Quit deriving (Eq)

data MenuState = MenuState {
    selectedOption :: MenuOption,
    infoWindow :: Bool  -- Estado da janela de informações
    }

initialState :: MenuState
initialState = MenuState { selectedOption = Play, infoWindow = False}

renderMenu :: Picture -> MenuState -> Picture
renderMenu background menuState = pictures [
    background,
    translate (-220) 105 $ scale 0.52 0.52 $ drawOption Play (selectedOption menuState == Play),
    translate (-220) (-5) $ scale 0.5 0.5 $ drawOption Info (selectedOption menuState == Info),
    translate (-220) (-115) $ scale 0.5 0.5 $ drawOption Quit (selectedOption menuState == Quit),
    infoBox (infoWindow menuState)  -- Renderiza a janela de informações se estiver aberta
  ]

drawOption :: MenuOption -> Bool -> Picture
drawOption option selected = color (if selected then dark green else white) $ text $ case option of
    Play    -> "Play"
    Info    -> "Info"
    Quit    -> "Quit"

handleInput :: Event -> MenuState -> MenuState
handleInput (EventKey (SpecialKey KeyDown) Down _ _) menuState =
    menuState { selectedOption = nextOption (selectedOption menuState) }
handleInput (EventKey (SpecialKey KeyUp) Down _ _) menuState =
    menuState { selectedOption = prevOption (selectedOption menuState) }
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) menuState =
    case selectedOption menuState of
        --startGame ...
        Info -> menuState { infoWindow = not (infoWindow menuState) }  -- Abre ou fecha a janela de informações
        Quit -> error "Game window closed... Até à próxima!"  -- Encerra o programa ao selecionar Quit
        _ -> menuState
handleInput _ menuState = menuState  -- Caso padrão para qualquer outro evento, retorna o estado do menu inalterado

{-startGame :: MenuState -> MenuState
startGame menuState = do
    putStrLn "Starting game!"
    -- Adicione a lógica para iniciar o jogo aqui
    menuState -- Retorne o novo estado do menu após iniciar o jogo-}

infoBox :: Bool -> Picture
infoBox False = Blank  -- Janela de informações oculta
infoBox True = pictures [
    translate 0 0 $ color (greyN 0.43) $ rectangleSolid 900 700,
    translate 0 0 $ color black $ rectangleWire 900 700,
    translate (-300) 280 $ scale 0.35 0.35 $ color black $ text "Informacoes Importantes /!\\",
    translate (-300) 290 $ color black $ text "______",
    translate (-350) 150 $ scale 0.3 0.3 $ color white $ text "No ambito da UC de Laboratorios",
    translate (-350) 105 $ scale 0.3 0.3 $ color white $ text "de Informatica I foi nos proposta",
    translate (-350) 65 $ scale 0.3 0.3 $ color white $ text "a realizacao de um remake do",
    translate (-350) 25 $ scale 0.3 0.3 $ color white $ text "famoso jogo \"Donkey Kong\" (1981).",

    translate (-350) (-90) $ scale 0.3 0.3 $ color white $ text "Este Jogo foi desenvolvido por:",
    translate (-250) (-130) $ scale 0.2 0.2 $ color white $ text "Goncalo Castro LEI a107337",
    translate (-250) (-170) $ scale 0.2 0.2 $ color white $ text "Luis Felicio LEI a106913",    
    translate (-150) (-300) $ scale 0.2 0.2 $ color (greyN 0.2) $ text "Press Enter to close"
  ]

nextOption :: MenuOption -> MenuOption
nextOption Play = Info
nextOption Info = Quit
nextOption Quit = Play

prevOption :: MenuOption -> MenuOption
prevOption Play = Quit
prevOption Info = Play
prevOption Quit = Info

window :: Display
window = InWindow "Konkey Donk" (1250, 1000) (10, 10)

main :: IO ()
main = do p1 <- loadBMP "out.bmp"
          play window black 60 initialState (renderMenu (translate 0 0 $ scale 0.45 0.45 p1)) handleInput (\_ -> id)