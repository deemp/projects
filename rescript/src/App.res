%%raw(`import './App.css';`)
@module("./logo.svg") external logo: string = "default"

type todo = {
  title: string,
  isDone: bool,
}

type state = {todoList: array<todo>, inputValue: string}

let initialState: state = {
  todoList: [],
  inputValue: "",
}

type actions =
  | AddTodo
  | ClearTodos
  | InputChanged(string)
  | SwitchDone(int)

let reducer = (state, action) => {
  switch action {
  | AddTodo => {
      ...state,
      todoList: state.todoList->Js.Array2.concat([
        {
          title: state.inputValue,
          isDone: false,
        },
      ]),
    }
  | ClearTodos => {
      ...state,
      todoList: [],
    }
  | InputChanged(newValue) => {
      ...state,
      inputValue: newValue,
    }
  | SwitchDone(index) => {
      ...state,
      todoList: state.todoList->Belt.Array.mapWithIndex((i, todo) => {
        if i == index {
          {
            ...todo,
            isDone: !todo.isDone,
          }
        } else {
          todo
        }
      }),
    }
  }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState)

  let handleInput = e => {
    let newValue = ReactEvent.Form.target(e)["value"]
    newValue->InputChanged->dispatch
  }
  <div className="App">
    <h1> {"Todo Items"->React.string} </h1>
    {state.inputValue->React.string}
    <input value={state.inputValue} type_="text" onChange={handleInput} />
    <button onClick={_ => AddTodo->dispatch}> {"Add"->React.string} </button>
    <button onClick={_ => ClearTodos->dispatch}> {"Clear"->React.string} </button>
    {state.todoList
    ->Belt.Array.mapWithIndex((i, todo) => {
      <div
        onClick={_ => i->SwitchDone->dispatch}
        key={todo.title}
        style={ReactDOM.Style.make(
          ~background={todo.isDone ? "green" : "steelblue"},
          ~textDecoration={todo.isDone ? "line-through" : "initial"},
          ~color="white",
          ~padding="1rem",
          ~fontSize="1.5rem",
          ~margin="1rem 0",
          (),
        )}>
        {todo.title->React.string}
      </div>
    })
    ->React.array}
  </div>
}
