module Main where

import Data.Date
import Data.Date.Component
import Data.Foreign.Class
import Prelude
import WebSocket

import Control.Coroutine as CR
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, nowDate)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Data.Array (replicate, singleton, (!!))
import Data.Array (unionBy)
import Data.DateTime.Locale (LocalValue(LocalValue))
import Data.Either (Either(Left, Right), either)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Database.Postgres (ClientConfig, ConnectionInfo, DB, connectionInfoFromConfig, defaultPoolConfig, end, execute_, mkPool, query, queryOne, withClient)
import Database.Postgres as DP
import Database.Postgres.SqlValue (toSql)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(AttrName), ClassName(ClassName))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)

foreign import logMe :: forall a. a -> a
foreign import jsonParse :: String -> Foreign 
foreign import pushEvent ::forall m. String -> Date ->  Unit
foreign import getAllEvents :: forall eff. Date -> (Array String -> Eff eff Unit)-> Eff eff Unit
foreign import getEventData :: forall eff. Int -> Int -> Int-> (Array String)
main :: forall t179.
   Eff
     ( dom :: DOM
     , avar :: AVAR
     , err :: EXCEPTION
     , exception :: EXCEPTION
     , ref :: REF
     , ws :: WEBSOCKET
     , now :: NOW
     , db :: DB
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

getNow :: forall e. Aff (now :: NOW | e) Date
getNow = do
      (LocalValue _  date) <- liftEff nowDate
      pure date

wsConsumer :: forall eff. (Query ~> Aff (HA.HalogenEffects eff)) -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query = CR.consumer \msg -> do
	insertDataPS 2018 1 ""
        msgs <- getAllEventDataPS 2018 1 1 
        query $ H.action $ WebHook (singleton msg) 
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
	, eventData :: Array String
	}

data Query a = 
	Next a
 	| Prev a
  	| WebHook (Array String) a


toEnumL ::forall a. BoundedEnum a => Int -> a 
toEnumL a = unsafePartial $  fromJust $  toEnum a

data OMessage = 
	OutputMessage String

startOfMonth :: Int -> Month  -> Int
startOfMonth year month = fromEnum $ weekday $ canonicalDate (toEnumL year)  month (toEnumL 1) 

tableView :: State -> H.ComponentHTML Query
tableView state = 
      HH.table [HH.attr (AttrName "width") "100%"] 
	 [HH.tbody_ $ map (makeRow (startOfMonth state.yearSelected state.monthSelected )) [1,2,3,4,5,6]]
      where
	    makeRow startDay index = 
		  HH.tr [HH.attr (AttrName "height") "50" ]
                     [ HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 1 startDay state).value) )]
									       [ HH.div [HP.class_ $ ClassName "Left_Text"] [HH.text (fromMaybe "" (if index == 1 then state.eventData !! 0 else state.eventData !! (1+7*index)))]
		       ,HH.div [HP.class_ $ ClassName "Right_Text"] [ HH.text ((getText index 1 startDay state).value)]
	               ]   
	              , HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 2 startDay state).value) )] [HH.div [HP.class_ $ ClassName "Left_Text"][HH.text (fromMaybe "" (if index == 2 then state.eventData !! 1 else state.eventData !! (2+7*index)))], HH.div [HP.class_ $ ClassName "Right_Text"] [HH.text ((getText index 2 startDay state).value)]]
                     , HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 3 startDay state).value) )] [HH.div [HP.class_ $ ClassName "Left_Text"][HH.text (fromMaybe "" (if index == 3 then state.eventData !! 2 else state.eventData !! (3+7*index)))], HH.div [HP.class_ $ ClassName "Right_Text"] [HH.text ((getText index 3 startDay state).value)]]
		     , HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 4 startDay state).value) )] [HH.div [HP.class_ $ ClassName "Left_Text"][HH.text (fromMaybe "" (if index == 4 then state.eventData !! 3 else state.eventData !! (4+7*index)))], HH.div [HP.class_ $ ClassName "Right_Text"] [HH.text ((getText index 4 startDay state).value)]]
                     , HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 5 startDay state).value) )] [HH.div [HP.class_ $ ClassName "Left_Text"][HH.text (fromMaybe "" (if index == 5 then state.eventData !! 4 else state.eventData !! (5+7*index)))], HH.div [HP.class_ $ ClassName "Right_Text"] [HH.text (getText index 5 startDay state).value]]
		     , HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 6 startDay state).value) )]  [HH.div [HP.class_ $ ClassName "Left_Text"][HH.text (fromMaybe "" (if index == 6 then state.eventData !! 5 else state.eventData !! (6+7*index)))], HH.div [HP.class_ $ ClassName "Right_Text"][HH.text (getText index 6 startDay state).value]]
                     , HH.td [HH.attr (AttrName "width")"15%",HH.attr (AttrName "align") "right",HP.ref $ H.RefLabel ("DayStage" <> ((getText index 7 startDay state).value) )]  [HH.div [HP.class_ $ ClassName "Left_Text"][HH.text (fromMaybe "" (if index == 7 then state.eventData !! 6 else state.eventData !! (7+7*index)))], HH.div [HP.class_ $ ClassName "Right_Text"] [HH.text (getText index 7 startDay state).value]]
		     ]

type StrInt = { value :: String
              , index :: Int
	      }

getText :: Int -> Int -> Int -> State ->StrInt 
getText index val startDay state = if index == 1 then if startDay == val then {value: (show state.monthSelected <> show 1),index : 1} else if startDay <  val then {value: show (val-startDay +1),index: (val-startDay+1)} else if val == 1 then {value: (show $ getPreviousMonth state.monthSelected),index:val} else {value:"",index:(-1)} else if index ==2 && startDay /= 1 then {value:show (8-(startDay-val)),index: (8-startDay+val)} else if startDay >= val then if state.endDate >= (((index-1)*8)-(index-2)-(startDay-val)) then {value: show $ (((index-1)*8)-(index-2)-(startDay-val)), index: (((index-1)*8)-(index-2)-(startDay-val))} else {value:"",index:(-1)} else if state.endDate >= (val-startDay+1 +(index-1)*7) then {value: show $ (val-startDay+1 +(index-1)*7), index: (val-startDay+1 + (index-1)*7)} else {value: "",index:(-1)}  

getPreviousMonth :: Month -> Month
getPreviousMonth monthSelected = if monthSelected == January then December else toEnumL $ fromEnum monthSelected -1

getNextMonth :: Month -> Month 
getNextMonth monthSelected = if monthSelected == December then January else  toEnumL $ fromEnum monthSelected +1

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
			    , HH.button [HE.onClick $ HE.input_ Next, HP.class_ $ ClassName "next_button"][HH.text "NEXT>>"]
			    , HH.button [HE.onClick $ HE.input_ Prev, HP.class_ $ ClassName "prev_button"] [HH.text "<<PREV"]
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
				      eventData = getEventData newYear (startOfMonth newYear newMonth) (fromEnum newMonth)
		                  in st {monthSelected = newMonth, yearSelected = newYear, endDate = lastDay, eventData = eventData}
				 )
			      )
		     pure next
	        eval (Prev next) = do
		     H.modify (\st ->
				 (let newMonth = if st.monthSelected == January then December else getPreviousMonth st.monthSelected 			              
				      newYear = if newMonth == December then st.yearSelected -1 else st.yearSelected
			              lastDay = fromEnum $ lastDayOfMonth (toEnumL newYear) newMonth
	                              eventData = getEventData newYear (startOfMonth newYear newMonth) (fromEnum newMonth)
		                  in st {monthSelected = newMonth, yearSelected = newYear, endDate = lastDay, eventData = eventData}
				 )
			      )
		     pure next
	        eval (WebHook msg next) = do
		     H.modify ( _ {eventData = msg }) 
		     pure next
    		
		initialStateData :: State
  		initialStateData = { monthSelected: January, yearSelected: 2018, endDate:31, eventData : getEventData 2018 (startOfMonth 2018 (toEnumL 1)) 1}

clientConfig :: ClientConfig
clientConfig =
   { host : "localhost"
   , database : "github"
   , port : 5432
   , user : "postgres"
   , password : "admin"
   , ssl : false
   }

connectionInfo :: ConnectionInfo
connectionInfo = connectionInfoFromConfig clientConfig defaultPoolConfig

getAllEventDataPS ::forall e. Int -> Int -> Int -> Aff (HA.HalogenEffects e) (Array String)
getAllEventDataPS year start month= do
   pool <- liftEff $ mkPool connectionInfo
   events <- withClient pool \c -> do
      queryOne (DP.Query "select * from eventData where year = $1 and month = $2" :: DP.Query (Array String)) [toSql year, toSql month] c
   (pure $ end pool) *> pure $ unionBy (\_ _ -> false) (replicate (start-1) "") (fromMaybe [""] events) 

insertDataPS :: forall e. Int -> Int -> String -> Aff (HA.HalogenEffects  e) Unit
insertDataPS year month event = do
   pool <- liftEff $ mkPool connectionInfo
   withClient pool \c -> do
      execute_ (DP.Query "insert into eventData values(year,month,event)") c
      liftEff $ end pool

