## Naming conventions

Anything that extends other libraries or mimics their functionality for other types should follow naming conventions of that library.

Anything from the object world (and members of record types) should be lowercased_underscored, like so:

    let local_variable = 10
    let some_function x = x + 50
    type RecordType = {
        field: string
        another_field: int
        some_member_function: unit -> int
    } 

Anything from the type world (and type constructors) should be PascalCased, like so:

    type HelloData = {
        field: string
        another_field: int
        some_member_function: unit -> int
    }

    type MyAwesomeUnion = 
    | Hello of HelloData
    | AnotherHello