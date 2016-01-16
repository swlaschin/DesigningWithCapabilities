type CustomerId = CustomerId of int
type CustomerProfile = {name: string }

// Define some "access tokens" (really just types)
type GetCustomerProfileToken = 
    { customerIdToGet : CustomerId }

type UpdateCustomerPasswordToken = 
    { customerIdToUpdatePassword : CustomerId }

// =========================
// The authorization module
// =========================
module Authorization =
    
    let getCustomerToken myIdentity customerId =
        if myIdentity = 0 then
            // if a superuser, give them an Access token
            Some { customerIdToGet = CustomerId customerId }
        elif myIdentity = customerId then
            // if the customer is updating their own data, give them an Access token
            Some { customerIdToGet = CustomerId customerId }
        else
            // otherwise, deny
            None

    let updatePasswordToken myIdentity customerId =
        if myIdentity = customerId then
            // if the customer is updating their own password, give them an Access token
            Some { customerIdToUpdatePassword = CustomerId customerId }
        else
            // otherwise, deny
            None

// =========================
// The database layer
// =========================
let dbGetProfile token =
    // parse the token to get the customerId to access
    let (CustomerId id) = token.customerIdToGet

    // do the work
    printfn "DB: Profile for customer %i fetched" id
    match id with
    | 1 -> {name="Alice"}
    | 2 -> {name="Bob"}
    | _ -> {name="Someone else"}

let dbUpdatePassword token newPassword =
    // parse the token to get the customerId to access
    let (CustomerId id) = token.customerIdToUpdatePassword
    
    // do the work
    printfn "DB: Password for customer %i updated to '%s'" id newPassword

// =========================
// The email service
// =========================

let smtpSendEmail getProfileCap =
    let profile = getProfileCap()
    printfn "EMAIL: Sent email to %s" profile.name

// =========================
// The controller for getting profiles
// =========================

let profileController identity (CustomerId customerId) =

    // attempt to get an access token
    let tokenOpt = 
        Authorization.getCustomerToken identity customerId 
    
    match tokenOpt with
    // no token returned? 
    | None ->
        printfn "CONTROLLER: Don't have permission to read profile" 
    // a valid token returned? 
    | Some token ->
        // give the token to the database service
        let profile = dbGetProfile token 
        printfn "CONTROLLER: Fetched profile for '%s'" profile.name
        // create a new capability for delegating 
        let capForEmail() = dbGetProfile token 
        smtpSendEmail capForEmail

// ========================= 
// test profile access
// =========================

let aliceController = profileController 1
// (CustomerId -> unit)

aliceController (CustomerId 1)
(*
DB: Profile for customer 1 fetched
CONTROLLER: Fetched profile for 'Alice'
DB: Profile for customer 1 fetched
EMAIL: Sent email to Alice
*)

aliceController (CustomerId 2)
(*
CONTROLLER: Don't have permission to read profile
*)

// superuser
let adminController = profileController 0
// (CustomerId -> unit)

adminController (CustomerId 1)
(*
DB: Profile for customer 1 fetched
CONTROLLER: Fetched profile for 'Alice'
DB: Profile for customer 1 fetched
EMAIL: Sent email to Alice
*)

adminController (CustomerId 2)
(*
DB: Profile for customer 2 fetched
CONTROLLER: Fetched profile for 'Bob'
DB: Profile for customer 2 fetched
EMAIL: Sent email to Bob
*)


// ========================= 
// The controller for updating passwords
// =========================

let passwordController identity (CustomerId customerId) newPassword =

    // attempt to get an access token
    let tokenOpt = 
        Authorization.updatePasswordToken identity customerId 
    
    match tokenOpt with
    | None ->
        printfn "CONTROLLER: Don't have permission to change password" 
    | Some token ->
        // give the token to the database service
        dbUpdatePassword token newPassword
        printfn "CONTROLLER: Updated password for customer %i" customerId


// ========================= 
// test password updates
// =========================

let alicePasswordController = passwordController 1
// (CustomerId -> string -> unit)

alicePasswordController (CustomerId 1) "password"
(*
DB: Password for customer 1 updated to 'password'
CONTROLLER: Updated password for customer 1
*)

alicePasswordController (CustomerId 2) "password"
(*
CONTROLLER: Don't have permission to change password
*)

//super user
let adminPasswordController = passwordController 0
// (CustomerId -> string -> unit)

adminPasswordController (CustomerId 1) "password"
(*
CONTROLLER: Don't have permission to change password
*)


