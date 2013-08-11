module ApiProtocol

type TrainRequest = { 
    size: int option
    operators: array<string> option
    }

type TrainingProblem = {
     challenge: string
     id: string
     size: int
     operators: array<string>
     }

type EvalRequest = {
    id: string option
    program: string option
    arguments: array<string>
}

type EvalResponse = {
    status: string 
    outputs: array<string> option  
    message: string option
}

type Guess = {
    id: string
    program: string
    }

type GuessResponse = {
    status: string
    values: array<string> option
    message: string option
    lightning: bool option
    }

// partial description
type Status = {
    easyChairId: string
    contestScore: int
    lightningScore: int
    trainingScore: int
    mismatches: int
    numRequests: int
    }

type Problem = {
    id: string
    size: int
    operators: array<string>
    solved: bool option
    timeLeft: int option   
}

