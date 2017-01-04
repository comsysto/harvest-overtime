module Harvest.TaskAPI
    exposing
        ( Task
        , tasksDecoder
        , taskDecoder
        , getAllTasks
        , getTask
        , createTask
        , deleteTask
        , updateTask
        , TaskAssignment
        , taskAssignmentsDecoder
        , taskAssignmentDecoder
        , getTasksAssignedToProject
        , getTaskAssignment
        , assignTaskToAProject
        , createNewTaskAndAssignItToProject
        , removeTaskFromProject
        , updateTaskAssignment
        )

import Date exposing (Date)
import Json.Encode as JE
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Decode.Extra exposing (date)
import Json.Encode.Extra as JEE exposing (maybe)
import Http exposing (..)
import Dict exposing (Dict)


type alias Task =
    { id : Int
    , name : String
    , billableByDefault : Bool
    , isDefault : Bool
    , defaultHourlyRate : Float
    , deactivated : Bool
    , createdAt : Maybe Date
    , updatedAt : Maybe Date
    }


type alias TaskAssignment =
    { id : Int
    , projectId : Int
    , taskId : Int
    , billable : Bool
    , deactivated : Bool
    , hourlyRate : Float
    , budget : Maybe Float
    , estimate : Maybe Float
    , createdAt : Date
    , updatedAt : Date
    }



{- decoders -}


tasksDecoder : Decoder (List Task)
tasksDecoder =
    list (field "task" taskDecoder)


taskDecoder : Decoder Task
taskDecoder =
    decode Task
        |> required "id" int
        |> required "name" string
        |> required "billable_by_default" bool
        |> required "is_default" bool
        |> required "default_hourly_rate" float
        |> required "deactivated" bool
        |> required "created_at" (nullable date)
        |> required "updated_at" (nullable date)


taskAssignmentsDecoder : Decoder (List TaskAssignment)
taskAssignmentsDecoder =
    list (field "task_assignment" taskAssignmentDecoder)


taskAssignmentDecoder : Decoder TaskAssignment
taskAssignmentDecoder =
    decode TaskAssignment
        |> required "id" int
        |> required "project_id" int
        |> required "task_id" int
        |> required "billable" bool
        |> required "deactivated" bool
        |> required "hourly_rate" float
        |> required "budget" (nullable float)
        |> required "estimate" (nullable float)
        |> required "created_at" date
        |> required "updated_at" date



-- GET https://YOURACCOUNT.harvestapp.com/tasks


getAllTasks : String -> String -> Dict String String -> Request (List Task)
getAllTasks accountId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = createUrl accountId token params
        , body = emptyBody
        , expect = expectJson tasksDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- GET https://YOURACCOUNT.harvestapp.com/tasks/{task_id}


getTask : String -> Int -> String -> Request Task
getTask accountId taskId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/tasks/" ++ (toString taskId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson taskDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- POST https://YOURACCOUNT.harvestapp.com/tasks


createTask : String -> String -> Task -> Request String
createTask accountId token task =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/tasks?access_token=" ++ token
    in
        request
            { method = "POST"
            , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
            , url = url
            , body = jsonBody <| encodeTask task
            , expect = expectString
            , timeout = Nothing
            , withCredentials = False
            }



-- DELETE https://YOURACCOUNT.harvestapp.com/tasks/{task_id}


deleteTask : String -> Int -> String -> Request String
deleteTask accountId taskId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/tasks/" ++ (toString taskId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- PUT https://YOURACCOUNT.harvestapp.com/tasks/{task_id}


updateTask : String -> Task -> String -> Request String
updateTask accountId task token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/tasks/" ++ (toString task.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeTask task
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- Task assignments -}
--GET https://YOURACCOUNT.harvestapp.com/projects/{project_id}/task_assignments


getTasksAssignedToProject : String -> Int -> String -> Dict String String -> Request (List TaskAssignment)
getTasksAssignedToProject accountId projectId token params =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = taskAssignmentUrl accountId projectId token params
        , body = emptyBody
        , expect = expectJson taskAssignmentsDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- GET https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/task_assignments/{TASK_ASSIGNMENT_ID}


getTaskAssignment : String -> Int -> Int -> String -> Request TaskAssignment
getTaskAssignment accountId projectId assignmentId token =
    request
        { method = "GET"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "task_assignments/" ++ (toString assignmentId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectJson taskAssignmentDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- POST https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/task_assignments


assignTaskToAProject : String -> Int -> Int -> String -> Request String
assignTaskToAProject accountId taskId projectId token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/task_assignments?access_token=" ++ token
        , body = jsonBody <| encodeTaskAssignmentId taskId
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- POST https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/task_assignments/add_with_create_new_task


createNewTaskAndAssignItToProject : String -> Int -> String -> String -> Request String
createNewTaskAndAssignItToProject accountId projectId taskName token =
    request
        { method = "POST"
        , headers = [ header "Accept" "application/json", header "Content-Type" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/task_assignments/add_with_create_new_task?access_token=" ++ token
        , body = jsonBody <| encodeTaskName taskName
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- DELETE https://YOURACCOUNT.harestapp.com/projects/{PROJECT_ID}/task_assignments/{TASK_ASSIGNMENT_ID}


removeTaskFromProject : String -> Int -> Int -> String -> Request String
removeTaskFromProject accountId projectId assignmentId token =
    request
        { method = "DELETE"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/task_assignments/" ++ (toString assignmentId) ++ "?access_token=" ++ token
        , body = emptyBody
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- PUT https://YOURACCOUNT.harvestapp.com/projects/{PROJECT_ID}/task_assignments/{TASK_ASSIGNMENT_ID}


updateTaskAssignment : String -> TaskAssignment -> String -> Request String
updateTaskAssignment accountId assignment token =
    request
        { method = "PUT"
        , headers = [ header "Accept" "application/json" ]
        , url = "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString assignment.projectId) ++ "/task_assignments/" ++ (toString assignment.id) ++ "?access_token=" ++ token
        , body = jsonBody <| encodeTaskAssignment assignment
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }



{- helpers -}


encodeTask : Task -> JE.Value
encodeTask p =
    JE.object
        [ ( "task"
          , JE.object
                [ ( "name", JE.string p.name )
                , ( "billable_by_default", JE.bool p.billableByDefault )
                , ( "is_default", JE.bool p.isDefault )
                , ( "default_hourly_rate", JE.float p.defaultHourlyRate )
                , ( "deactivated", JE.bool p.deactivated )
                ]
          )
        ]


encodeTaskAssignment : TaskAssignment -> JE.Value
encodeTaskAssignment assignment =
    JE.object
        [ ( "task_assignment"
          , JE.object
                [ -- ( "name", JE.string assignment.name ) WRONG DOCUMENTATION!!!
                  ( "billable", JE.bool assignment.billable )
                , ( "budget", JEE.maybe JE.float assignment.budget )
                , ( "deactivated", JE.bool assignment.deactivated )
                , ( "hourly_rate", JE.float assignment.hourlyRate )
                ]
          )
        ]


encodeTaskAssignmentId : Int -> JE.Value
encodeTaskAssignmentId taskId =
    JE.object
        [ ( "task"
          , JE.object
                [ ( "id", JE.int taskId ) ]
          )
        ]


encodeTaskName : String -> JE.Value
encodeTaskName name =
    JE.object
        [ ( "task"
          , JE.object
                [ ( "name", JE.string name ) ]
          )
        ]


createUrl : String -> String -> Dict String String -> String
createUrl accountId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/tasks?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p


taskAssignmentUrl : String -> Int -> String -> Dict String String -> String
taskAssignmentUrl accountId projectId token params =
    let
        url =
            "https://" ++ accountId ++ ".harvestapp.com/projects/" ++ (toString projectId) ++ "/task_assignments?access_token=" ++ token

        p =
            Dict.foldl (\key val agg -> agg ++ "&" ++ key ++ "=" ++ val) "" params
    in
        url ++ p
