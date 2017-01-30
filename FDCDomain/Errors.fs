[<AutoOpen>]
module FDCDomain.Errors

open System

type StringError =
| Missing
| NotASCIIString
| IncludesForbiddenCharacter of char
| MustNotBeShorterThan of int
| CouldntConvert of Exception

type TransportError =
| CouldntConnect of string

type ActionError =
| InvalidState
| InvalidAction
| DepsAreMissing
| TransportError of TransportError

type QueueError<'a> =
| CouldntConnect of 'a
