module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)



{-basic style for the h2 headers-}
headerStyle = style [("color","white"),
                     ("padding","0px 0px 0px 0px"),
                     ("margin","0px 0px 0px 0px")]

asdfText: Int -> String
asdfText num = case num of 
                      1 -> "asdf"
                      _ -> "asdf "++(asdfText (num-1))

main = div [][
          header[][{-below is style for top header, zeroed margin/padding to fit to sides-}
              h1 [style [("text-align","center"),("color","white"),("background-color","dodgerblue"),("margin","0"),("padding","0")]] [text "My Ugly Nonsense Webpage"]
            ]{-div secton for the botom half of the page, sets text color, background and padding to make the text spaced a certain distance from edge-}
          ,div [style [("color","white"),("background-color","tomato"),("padding","2em 5em 0em 12em")]][
            section[style [("float","right"),("text","white")]][{-float right to make it appear to the right of the next section-}
              h2[headerStyle][text "Useless List"],{-standard ordered list in html-}
              ol[][
                li [] [text "Link 1"]
                ,li [] [text "Link 2"]
                ,li [] [text "Link 3"]
                ,li [] [text "Link 4"]
                ,li [] [text "Link 5"]
                ,li [] [text "Link 6"]
                ,li [] [text "Link 7"]
                ,li [] [text "Link 8"]
                ,li [] [text "Link 9"]
                ,li [] [text "Link 10"]
                ]
              ]
            ,section [][{-main asdf paragraph, generates text from asdfText-}
              h2[headerStyle][text "First Section of Nonsense"]
              ,p [] [text (asdfText 324)]
             ]
            ,section[][
              h2 [headerStyle] [text "Second Section of More Nonsense"]
              ,p[][{-in order to get the haiku in new lines used div, alternative is the break tag in html <br>, floated to the left w/ clears to have text generally at the left edge-}
                div [style [("text-align","center"),("padding","0"),("margin","0"),("float","left"),("clear","left")]][
                  div[] [text "fat man sees small door"]
                  ,div[][text "he knows he cannot fit through"]
                  ,div[][text "tears flow free now"]
                  ]
                ]
             ]
            ,footer[style [("clear","left "),("margin","0"),("padding","1em 0 0 0")]][{-footer, pad top to give the proper spacing from the haiku and none for rest to stick to bottom-}
              div[style [("text-align","center")]][strong [] [text "Copyright NonsenseCompany @ 2018"]]{-Strong tag to make text bold in addition to being aligned to the center-}
             ]
            ]
          ]