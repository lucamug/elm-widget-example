module Route exposing (Route(..), Slug(..), maybeRoute, routeToString)

import Navigation
import UrlParser exposing ((</>))


-- ROUTING --


type Route
    = RouteHome
    | RouteWidgetChallenger
    | RouteWidgetCreateAccount
    | RouteWidgetCreateAccountWithEmailStep1
    | RouteWidgetCreateAccountWithEmailStep2
    | RouteWidgetCreateAccountWithEmailStep3
    | RouteWidgetCreateAccountWithPhoneStep1
    | RouteWidgetCreateAccountWithPhoneStep2
    | RouteWidgetCreateAccountWithPhoneStep3
    | RouteWidgetSignIn Slug
    | RouteFramework
    | RouteFramework2 Slug Slug


path :
    { challenger : String
    , codeSent : String
    , createAccount : String
    , emailSent : String
    , emailVerified : String
    , framework : String
    , phoneVerified : String
    , widthEmail : String
    , withPhone : String
    , signIn : String
    }
path =
    { createAccount = "create_account"
    , signIn = "signin"
    , framework = "framework"
    , challenger = "challenger"
    , widthEmail = "with_email"
    , withPhone = "with_phone"
    , emailSent = "email_sent"
    , codeSent = "code_sent"
    , emailVerified = "email_verified"
    , phoneVerified = "phone_verified"
    }


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map RouteHome (UrlParser.s "")
        , UrlParser.map RouteFramework (UrlParser.s path.framework)
        , UrlParser.map RouteFramework2 (UrlParser.s path.framework </> stateParser </> stateParser)
        , UrlParser.map RouteWidgetChallenger (UrlParser.s path.challenger)
        , UrlParser.map RouteWidgetCreateAccount (UrlParser.s path.createAccount)
        , UrlParser.map RouteWidgetCreateAccountWithEmailStep1 (UrlParser.s path.createAccount </> UrlParser.s path.widthEmail)
        , UrlParser.map RouteWidgetCreateAccountWithEmailStep2 (UrlParser.s path.createAccount </> UrlParser.s path.widthEmail </> UrlParser.s path.emailSent)
        , UrlParser.map RouteWidgetCreateAccountWithEmailStep3 (UrlParser.s path.createAccount </> UrlParser.s path.widthEmail </> UrlParser.s path.emailVerified)
        , UrlParser.map RouteWidgetCreateAccountWithPhoneStep1 (UrlParser.s path.createAccount </> UrlParser.s path.withPhone)
        , UrlParser.map RouteWidgetCreateAccountWithPhoneStep2 (UrlParser.s path.createAccount </> UrlParser.s path.withPhone </> UrlParser.s path.codeSent)
        , UrlParser.map RouteWidgetCreateAccountWithPhoneStep3 (UrlParser.s path.createAccount </> UrlParser.s path.withPhone </> UrlParser.s path.phoneVerified)
        , UrlParser.map RouteWidgetSignIn (UrlParser.s path.signIn </> stateParser)
        ]



-- Slug


type Slug
    = Slug String


slugToString : Slug -> String
slugToString (Slug slug) =
    slug


stateParser : UrlParser.Parser (Slug -> a) a
stateParser =
    UrlParser.custom "SLUG" (Ok << Slug)



-- INTERNAL --


routeRoot : String
routeRoot =
    "#/"


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                RouteHome ->
                    []

                RouteWidgetChallenger ->
                    [ path.challenger ]

                RouteWidgetCreateAccount ->
                    [ path.createAccount ]

                RouteWidgetCreateAccountWithEmailStep1 ->
                    [ path.createAccount, path.widthEmail ]

                RouteWidgetCreateAccountWithEmailStep2 ->
                    [ path.createAccount, path.widthEmail, path.emailSent ]

                RouteWidgetCreateAccountWithEmailStep3 ->
                    [ path.createAccount, path.widthEmail, path.emailVerified ]

                RouteWidgetCreateAccountWithPhoneStep1 ->
                    [ path.createAccount, path.withPhone ]

                RouteWidgetCreateAccountWithPhoneStep2 ->
                    [ path.createAccount, path.withPhone, path.codeSent ]

                RouteWidgetCreateAccountWithPhoneStep3 ->
                    [ path.createAccount, path.withPhone, path.phoneVerified ]

                RouteWidgetSignIn slug ->
                    [ path.signIn, slugToString slug ]

                RouteFramework ->
                    [ path.framework ]

                RouteFramework2 slug1 slug2 ->
                    [ path.framework, slugToString slug1, slugToString slug2 ]
    in
    routeRoot ++ String.join "/" pieces



-- PUBLIC HELPERS --


maybeRoute : Navigation.Location -> Maybe Route
maybeRoute location =
    if String.isEmpty location.hash then
        Just RouteHome
    else
        UrlParser.parseHash route location
