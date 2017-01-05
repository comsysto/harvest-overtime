module Harvest.InvoiceAPI
    exposing
        ( Invoice
        , invoicesDecoder
        , invoiceDecoder
        , getAllInvoices
        , getInvoice
        , deleteInvoice
        , updateInvoice
        , createInvoice
        )

import Date exposing (Date)
import Date.Extra exposing (toFormattedString)
import Json.Encode as JE
import Json.Encode.Extra as JEE exposing (maybe)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Http exposing (..)
import Dict exposing (Dict)


type alias Invoice =
    { id : Int
    , clientId : Int
    , clientName : String
    , number : Maybe Int
    , periodStart : Date
    , periodEnd : Date
    , amount : Float
    , currency : String
    , notes : Maybe String
    , kind : Maybe String
    , projectsToInvoice : Maybe String
    , issuedAt : Date
    , dueAmount : Float
    , dueAt : Date
    , dueAtHumanFormat : String
    , subject : Maybe String
    , discount : Maybe String
    , discountAmount : Maybe Float
    , purchaseOrder : String
    , tax : Float
    , taxAmount : Float
    , tax2 : Maybe Float
    , tax2Amount : Float
    , estimateId : Maybe Int
    , recurringInvoiceId : Maybe Int
    , clientKey : Maybe String
    , retainerId : Maybe Int
    , createdById : Maybe Int
    , state : Maybe String
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }



{- Decoders -}


invoicesDecoder : Decoder (List Invoice)
invoicesDecoder =
    list (field "invoice" invoiceDecoder)


invoiceDecoder : Decoder Invoice
invoiceDecoder =
    decode Invoice
        |> required "id" int
        |> required "client_id" int
        |> required "client_name" string
        |> required "number" (nullable int)
        |> required "period_start" date
        |> required "period_end" date
        |> required "amount" float
        |> required "currency" string
        |> required "notes" (nullable string)
        |> required "kind" (nullable string)
        |> required "projects_to_invoice" (nullable string)
        |> required "issued_at" date
        |> required "due_amount" float
        |> required "due_at" date
        |> required "due_at_human_format" string
        |> required "subject" (nullable string)
        |> required "discount" (nullable string)
        |> required "discount_amount" (nullable float)
        |> required "purchase_order" string
        |> required "tax" float
        |> required "tax_amount" float
        |> required "tax2" (nullable float)
        |> required "tax2_amount" float
        |> required "estimate_id" (nullable int)
        |> required "recurring_invoice_id" (nullable int)
        |> required "client_key" (nullable string)
        |> required "retainer_id" (nullable int)
        |> required "created_by_id" (nullable int)
        |> required "state" (nullable string)
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)



-- GET https://YOURACCOUNT.harvestapp.com/invoices


getAllInvoices : String -> String -> Dict String String -> Request (List Invoice)
getAllInvoices accountId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrl accountId token params
        , body = emptyBody
        , expect = expectJson invoicesDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- GET https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}


getInvoice : String -> Int -> String -> Request Invoice
getInvoice accountId invoiceId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson invoiceDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- DELETE https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}


deleteInvoice : String -> Int -> String -> Request String
deleteInvoice accountId invoiceId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoiceId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- PUT https://YOURACCOUNT.harvestapp.com/invoices/{INVOICEID}


updateInvoice : String -> Invoice -> String -> Request String
updateInvoice accountId invoice token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices/" ++ (toString invoice.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeInvoice invoice
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- POST https://YOURACCOUNT.harvestapp.com/invoices


createInvoice : String -> String -> Invoice -> Request String
createInvoice accountId token invoice =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/invoices?access_token=" ++ token
        , body = jsonBody <| encodeInvoice invoice
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Helpers -}


encodeInvoice : Invoice -> JE.Value
encodeInvoice c =
    JE.object
        [ ( "invoice"
          , JE.object
                [ ( "due_at_human_format", JE.string c.dueAtHumanFormat )
                , ( "currency", JE.string c.currency )
                , ( "discount", JEE.maybe JE.string c.discount )
                , ( "discount_amount", JEE.maybe JE.float c.discountAmount )
                , ( "issued_at", JE.string (toFormattedString "yyyy-MM-dd" c.issuedAt) )
                , ( "subject", JEE.maybe JE.string c.subject )
                , ( "notes", JEE.maybe JE.string c.notes )
                , ( "number", JEE.maybe JE.int c.number )
                , ( "kind", JEE.maybe JE.string c.kind )
                , ( "projects_to_invoice", JEE.maybe JE.string c.kind )
                , ( "period-start", JE.string (toFormattedString "yyyy-MM-dd" c.periodStart) )
                , ( "period_end", JE.string (toFormattedString "yyyy-MM-dd" c.periodEnd) )
                , ( "purchase_order", JE.string c.purchaseOrder )
                , ( "tax", JE.float c.tax )
                , ( "tax2", JEE.maybe JE.float c.tax2 )
                ]
          )
        ]



{-
   Belongs to invoice, but Harvest API docs are not really clear. Won't support for now.

    import-hours	Hours to import into invoices. Options: all(import all hours), yes (import hours using period-start, period-end), no (do not import hours).
    import-expenses	Expenses to import into invoices. Options: all(import all expenses), yes (import expenses using expense-period-start, expense-period-end), no (do not import expenses).
    expense-summary-type	Summary type for expenses in an invoice. Options: project, people, category, detailed.
    expense-period-start	Date for included project expenses. (Example: 2015-04-22)
    expense-period-end	End date for included project expenses. (Example: 2015-05-22)
    csv-line-items	Used to create line items in free-form invoices. Entries should have their entries enclosed in quotes when they contain extra commas. This is especially important if you are using a number format which uses commas as the decimal separator.
-}


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/invoices?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
