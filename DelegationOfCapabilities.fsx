
type CustomerId = CustomerId of int
type CustomerProfile = {name: string }

// Define some capabilities
type GetCustomerProfile = 
    CustomerId -> CustomerProfile 

type UpdateCustomerProfile = 
    CustomerId -> CustomerProfile -> unit

// =========================
// The database layer
// =========================
let dbGetProfile (CustomerId id) =
    match id with
    | 1 -> {name="Alice"}
    | 2 -> {name="Bob"}
    | _ -> {name="Someone else"}

let dbUpdateProfile (CustomerId id) (profile:CustomerProfile) =
    printfn "DB: Profile for %s updated" profile.name


// =========================
// The email service
// =========================

let smtpSendEmail getProfileCap =
    let profile = getProfileCap()
    printfn "EMAIL: Sent email to %s" profile.name

// =========================
// The controller layer
// =========================

let profileController getProfile updateProfile id newProfile =
    updateProfile id newProfile 
    printfn "CONTROLLER: Updated profile for '%s'" newProfile.name

    // create a new capability for delegating
    let capForEmail() = getProfile id
    smtpSendEmail capForEmail


// =========================
// The top-level program which has full authority
// =========================


let createProfileController() =
    // get the capabilities 
    let getProfile = dbGetProfile 
    let updateProfile = dbUpdateProfile
    // inject the capabilities into the controller using partial application
    let controller = profileController getProfile updateProfile 
    controller 


// =========================
// test
// =========================

let controller = createProfileController()
// (CustomerId -> CustomerProfile -> unit)

controller (CustomerId 1) ({name="Alice"})
(*
DB: Profile for Alice updated
CONTROLLER: Updated profile for 'Alice'
EMAIL: Sent email to Alice
*)
