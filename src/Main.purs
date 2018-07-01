module Main where

import Data.Array
import Data.Date
import Data.Date.Component
import Prelude
import WebSocket

import Control.Coroutine (emit)
import Control.Coroutine as CR
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.HTML.Indexed (CSSPixel)
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Either (Either(..), either)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Maybe (Maybe(..), fromJust)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ClassName(..), Prop(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

foreign import logMe :: forall a. a -> a

main :: forall t179.
   Eff
     ( dom :: DOM
     , avar :: AVAR
     , err :: EXCEPTION
     , exception :: EXCEPTION
     , ref :: REF
     , ws :: WEBSOCKET
     | t179
     )
     Unit
main = do
 connection <- WS.create (WS.URL "ws://demos.kaazing.com/echo") [] 
 HA.runHalogenAff do 
   body <- HA.awaitBody
   io <- runUI component unit body
   CR.runProcess (wsProducerS connection CR.$$ wsConsumer io.query)
   pure unit

wsConsumer :: forall eff. (Query ~> Aff (HA.HalogenEffects eff)) -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query = CR.consumer \msg -> do
        query $ H.action $ WebHook msg 
        pure Nothing

wsProducer :: forall eff. WS.WebSocket -> CR.Producer String (Aff (HA.HalogenEffects (dom :: DOM | eff))) Unit
wsProducer socket = CRA.produce \emit ->
	EET.addEventListener
	    WSET.onMessage
            (listener emit)
	    false
            (WS.socketToEventTarget socket)
	where
	  listener emit = EET.eventListener \ev -> do
	     for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
	        for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
	            emit (Left msg)
          readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
          readHelper read = either (const Nothing) Just <<< runExcept <<< read <<< toForeign

wsProducerS :: forall eff. WS.WebSocket -> CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ws :: WEBSOCKET | eff)) Unit
wsProducerS socke = produce \emit -> do 
   Connection socket <- newWebSocket (URL "ws://localhost:8010") []
   {-- socket.onopen $= \event -> do --}
   {--     socket.send (Message "hello") --}
   {--     socket.send (Message "something") --}
   {--     socket.send (Message "goodbye") --}
   
   socket.onmessage $= \event -> do
      emit $ Left $ runMessage (runMessageEvent  event)

type State = 
	{ monthSelected :: Month 
        , yearSelected :: Int 
        , endDate :: Int
	}

data Query a = 
	Next a
 	| Prev a
  	| WebHook String  a


toEnumL ::forall a. BoundedEnum a => Int -> a 
toEnumL a = unsafePartial $  fromJust $  toEnum a

data OMessage = 
	OutputMessage String

startOfMonth :: State -> Int
startOfMonth state = fromEnum $ weekday $ canonicalDate (toEnumL state.yearSelected)  state.monthSelected (toEnumL 1) 

{-- days --}

tableView :: State -> H.ComponentHTML Query
tableView state = 
      HH.table [HH.attr (AttrName "width") "100%"] 
	 [HH.tbody_ $ map (makeRow (startOfMonth state )) [1,2,3,4,5,6]]
      where
	    makeRow startDay index = 
		  HH.tr []
                     [ HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 1 startDay state)] 
                     , HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 2 startDay state)]
                     , HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 3 startDay state)]
		     , HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 4 startDay state)]
                     , HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 5 startDay state)]
		     , HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 6 startDay state)]
                     , HH.td [HH.attr (AttrName "width")"15%"] [HH.text (getText index 7 startDay state)]
		     ]

getText :: Int -> Int -> Int -> State ->String 
getText index val startDay state = if index == 1 then if startDay == val then (show state.monthSelected <> show 1) else if startDay <  val then  show (val-startDay +1) else if val == 1 then (show $ getPreviousMonth state.monthSelected) else ""  else if index ==2 && startDay /= 1 then show (8-(startDay-val))  else if startDay >= val then if state.endDate >= (((index-1)*8)-(index-2)-(startDay-val)) then show $ (((index-1)*8)-(index-2)-(startDay-val)) else "" else if state.endDate >= (val-startDay+1 +(index-1)*7) then show $ (val-startDay+1 +(index-1)*7) else ""  


getPreviousMonth :: Month -> Month
getPreviousMonth monthSelected = ((toEnumL $ (fromEnum monthSelected -1)):: Month )

getNextMonth :: Month -> Month 
getNextMonth monthSelected =((toEnumL $ (fromEnum monthSelected) +1) :: Month)

tableViewDays :: State -> H.ComponentHTML Query
tableViewDays state = 
      HH.table [HH.attr( AttrName "width") "100%"]
	 [HH.tbody_ [
	    HH.tr []
	       	  [ HH.td [HH.attr (AttrName "width")"15%"] [HH.text "MON"]
		  , HH.td [HH.attr (AttrName "width")"15%"] [HH.text "TUE"]
                  , HH.td [HH.attr (AttrName "width")"15%"] [HH.text "WED"]
		  , HH.td [HH.attr (AttrName "width")"15%"] [HH.text "THUR"]
                  , HH.td [HH.attr (AttrName "width")"15%"] [HH.text "FRI"]
	          , HH.td [HH.attr (AttrName "width")"15%"] [HH.text "SAT"]
                  , HH.td [HH.attr (AttrName "width")"15%"] [HH.text "SUN"]
		  ]	
	 ]]

component ::forall m. H.Component HH.HTML Query Unit OMessage m
component = H.component
	{ initialState : const initialStateData
 	, render
  	, eval
   	, receiver : const Nothing
 	}
	where
       		render :: State -> H.ComponentHTML Query 
	 	render state =
                      HH.div_
	                 [ HH.h1 []
			    [ HH.text "Github Calendar" ]
			 , HH.div [HP.class_ $ ClassName "UnderLine"] 
                            [ HH.p [] [HH.text $ (show state.monthSelected) <> (show state.yearSelected)]
			    , HH.button [HE.onClick $ HE.input_ Next, HP.class_ $ ClassName "next_button"][HH.text "NEXT"]
			    , HH.button [HE.onClick $ HE.input_ Prev, HP.class_ $ ClassName "prev_button"] [HH.text "PREV"]
			    ]
		         , HH.div_
	                    [ HH.div []
	                        [ tableViewDays state
			        , tableView state
	                        ]
   		            ]
	                  ]
		
		eval :: Query ~> H.ComponentDSL State Query OMessage m
  		eval (Next next) = do
		     H.modify (\st ->
				 (let newMonth = if st.monthSelected == December then January else getNextMonth st.monthSelected 
			              newYear = if newMonth == January then st.yearSelected +1 else st.yearSelected
			              lastDay = fromEnum $ lastDayOfMonth (toEnumL newYear) newMonth
		                  in {monthSelected : newMonth, yearSelected : newYear, endDate : lastDay}
				 )
			      )
		     pure next
	        eval (Prev next) = do
		     H.modify (\st ->
				 (let newMonth = if st.monthSelected == January then December else getPreviousMonth st.monthSelected 			              
				      newYear = if newMonth == December then st.yearSelected -1 else st.yearSelected
			              lastDay = fromEnum $ lastDayOfMonth (toEnumL newYear) newMonth
		                  in {monthSelected : newMonth, yearSelected : newYear, endDate : lastDay}
				 )
			      )
		     pure next
	        eval (WebHook msg next) = do
	             {-- H.modify ( _ {monthSelected =e msg}) --} 
		     pure next
    		
		initialStateData :: State
  		initialStateData = { monthSelected: January, yearSelected: 2018, endDate:31}
