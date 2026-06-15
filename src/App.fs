module App

open Browser.Dom
open Browser.Types
open Types
open GraphEditor

let mutable state = emptyState 800.0 600.0
let customGates = ResizeArray<CustomGateDef>()
/// Stack of (parent state, custom gate node id, custom gate definition) for nested editing.
let navigationStack = ResizeArray<GraphState * NodeId * CustomGateDef>()

/// True if the given definition is the one currently being edited or one of its ancestors.
/// Placing such a gate inside itself (or a descendant of itself) would create a recursive
/// definition and crash the simulator.
let isForbiddenDef (def: CustomGateDef) =
    navigationStack |> Seq.exists (fun (_, _, d) -> d = def)

let setStyles (el: #Element) (styles: string) =
    el.setAttribute ("style", styles)

let createButton (label: string) (active: bool) (onClick: unit -> unit) =
    let btn = document.createElement ("button") :?> HTMLButtonElement
    btn.innerText <- label
    let bg = if active then "background:#2563eb;color:#ffffff;" else "background:#ffffff;color:#111827;"
    setStyles btn (sprintf "%s padding:6px 12px;margin:0 4px 0 0;cursor:pointer;border:1px solid #d1d5db;border-radius:4px;" bg)
    btn.onclick <- fun _ -> onClick ()
    btn

let createToolbar () =
    let container = document.createElement ("div")
    setStyles container "padding:8px;border-bottom:1px solid #e5e7eb;font-family:sans-serif;display:flex;align-items:center;"
    container

let createStatus () =
    let span = document.createElement ("span") :?> HTMLSpanElement
    setStyles span "margin-left:12px;color:#6b7280;font-size:14px;"
    span

let rec enterCustomGate (toolbar: HTMLElement) (status: HTMLSpanElement) (nodeId: NodeId) =
    match Map.tryFind nodeId state.Nodes with
    | Some n when isCustom n.GateType ->
        match n.GateType with
        | Custom def ->
            navigationStack.Add(state, nodeId, def)
            let maxNodeId =
                if Map.isEmpty def.InternalNodes then 0
                else def.InternalNodes |> Map.toSeq |> Seq.map fst |> Seq.max
            let maxEdgeId =
                if Map.isEmpty def.InternalEdges then 0
                else def.InternalEdges |> Map.toSeq |> Seq.map fst |> Seq.max
            state <-
                { emptyState state.CanvasWidth state.CanvasHeight with
                    Nodes = def.InternalNodes
                    Edges = def.InternalEdges
                    NextNodeId = maxNodeId + 1
                    NextEdgeId = maxEdgeId + 1
                    CustomStates = Map.empty }
                |> simulate
            updateToolbar toolbar status
        | _ -> ()
    | _ -> ()

and exitCustomGate (toolbar: HTMLElement) (status: HTMLSpanElement) =
    if navigationStack.Count > 0 then
        let parentState, nodeId, def = navigationStack.[navigationStack.Count - 1]
        navigationStack.RemoveAt(navigationStack.Count - 1)
        let inputNodeIds =
            state.Nodes
            |> Map.toSeq
            |> Seq.filter (fun (_, n) -> n.GateType = Input)
            |> Seq.map fst
            |> Set.ofSeq
        let outputNodeIds =
            state.Nodes
            |> Map.toSeq
            |> Seq.filter (fun (_, n) -> n.GateType = Output)
            |> Seq.map fst
            |> Set.ofSeq
        let oldInputIds = def.Inputs |> List.map fst |> Set.ofList
        let oldOutputIds = def.Outputs |> List.map fst |> Set.ofList

        // If the set of input/output nodes hasn't changed, preserve the existing
        // port mapping so moving nodes around inside the schematic doesn't silently
        // reorder the external ports. Only recompute ordering when nodes are added
        // or removed.
        let newInputs =
            if inputNodeIds = oldInputIds then def.Inputs
            else
                state.Nodes
                |> Map.toSeq
                |> Seq.filter (fun (_, n) -> n.GateType = Input)
                |> Seq.sortBy (fun (_, n) -> n.Y, n.X)
                |> Seq.map (fun (id, _) -> id, 0)
                |> Seq.toList
        let newOutputs =
            if outputNodeIds = oldOutputIds then def.Outputs
            else
                state.Nodes
                |> Map.toSeq
                |> Seq.filter (fun (_, n) -> n.GateType = Output)
                |> Seq.sortBy (fun (_, n) -> n.Y, n.X)
                |> Seq.map (fun (id, _) -> id, 0)
                |> Seq.toList

        def.Inputs <- newInputs
        def.Outputs <- newOutputs
        def.InternalNodes <- state.Nodes
        def.InternalEdges <- state.Edges
        // Drop external wires whose port no longer exists after editing the interface.
        let validInputPorts = Set.ofList [0 .. List.length def.Inputs - 1]
        let validOutputPorts = Set.ofList [0 .. List.length def.Outputs - 1]
        let edges =
            parentState.Edges
            |> Map.filter (fun _ e ->
                if e.Target = nodeId then Set.contains e.TargetPort validInputPorts
                elif e.Source = nodeId then Set.contains e.SourcePort validOutputPorts
                else true)
        state <-
            { parentState with
                CanvasWidth = state.CanvasWidth
                CanvasHeight = state.CanvasHeight
                Edges = edges
                CustomStates = Map.empty }
            |> simulate
        updateToolbar toolbar status

and updateToolbar (toolbar: HTMLElement) (status: HTMLSpanElement) =
    toolbar.innerHTML <- ""

    let title = document.createElement ("strong")
    title.innerText <- "Nodus"
    setStyles title "margin-right:16px;"
    toolbar.appendChild title |> ignore

    if navigationStack.Count > 0 then
        let backBtn = createButton "← Back" false (fun () ->
            exitCustomGate toolbar status)
        toolbar.appendChild backBtn |> ignore
        let scope =
            navigationStack
            |> Seq.map (fun (_, _, def) -> def.Name)
            |> String.concat " / "
        let scopeLabel = document.createElement ("span")
        scopeLabel.innerText <- sprintf "(%s)" scope
        setStyles scopeLabel "margin-right:16px;color:#6b7280;font-size:14px;"
        toolbar.appendChild scopeLabel |> ignore

    let refresh () = updateToolbar toolbar status

    let selectBtn = createButton "Select" (state.Mode = Select) (fun () -> state <- setMode Select state; refresh ())
    let addAndBtn = createButton "AND" (state.Mode = AddAnd) (fun () -> state <- setMode AddAnd state; refresh ())
    let addOrBtn = createButton "OR" (state.Mode = AddOr) (fun () -> state <- setMode AddOr state; refresh ())
    let addNandBtn = createButton "NAND" (state.Mode = AddNand) (fun () -> state <- setMode AddNand state; refresh ())
    let addInputBtn = createButton "Input" (state.Mode = AddInput) (fun () -> state <- setMode AddInput state; refresh ())
    let addOutputBtn = createButton "Output" (state.Mode = AddOutput) (fun () -> state <- setMode AddOutput state; refresh ())
    let addEdgeBtn = createButton "Wire" (state.Mode = AddEdge) (fun () -> state <- setMode AddEdge state; refresh ())
    toolbar.appendChild selectBtn |> ignore
    toolbar.appendChild addAndBtn |> ignore
    toolbar.appendChild addOrBtn |> ignore
    toolbar.appendChild addNandBtn |> ignore
    toolbar.appendChild addInputBtn |> ignore
    toolbar.appendChild addOutputBtn |> ignore
    toolbar.appendChild addEdgeBtn |> ignore

    if customGates.Count > 0 then
        let spacer = document.createElement ("span")
        setStyles spacer "width:16px;display:inline-block;"
        toolbar.appendChild spacer |> ignore
        customGates
        |> Seq.iter (fun def ->
            let isActive =
                match state.Mode with
                | AddCustom activeDef -> activeDef.Name = def.Name
                | _ -> false
            let forbidden = isForbiddenDef def
            let label = if forbidden then def.Name + " 🔒" else def.Name
            let btn = createButton label isActive (fun () ->
                if not forbidden then
                    state <- setMode (AddCustom def) state
                    refresh ())
            if forbidden then
                btn.setAttribute ("disabled", "true")
                setStyles btn "opacity:0.5;cursor:not-allowed;"
            toolbar.appendChild btn |> ignore)

    let spacer = document.createElement ("span")
    setStyles spacer "width:16px;display:inline-block;"
    toolbar.appendChild spacer |> ignore

    let clearBtn = createButton "Clear" false (fun () -> state <- clearGraph state; refresh ())
    let deleteBtn = createButton "Delete Selected" false (fun () -> state <- deleteSelected state; refresh ())
    let renameBtn = createButton "Rename Selected" false (fun () ->
        if state.SelectedNodes.Count > 0 then
            let firstLabel =
                state.SelectedNodes
                |> Set.toSeq
                |> Seq.choose (fun id ->
                    match Map.tryFind id state.Nodes with
                    | Some n -> Some n.Label
                    | None -> None)
                |> Seq.tryHead
                |> Option.defaultValue ""
            match window.prompt ("Rename selected nodes:", firstLabel) with
            | null -> ()
            | newLabel ->
                let nodes =
                    state.SelectedNodes
                    |> Set.fold (fun (m: Map<NodeId, Node>) id ->
                        match Map.tryFind id m with
                        | Some n -> Map.add id { n with Label = newLabel } m
                        | None -> m) state.Nodes
                state <- { state with Nodes = nodes }
                refresh ())
    let fixedBtn = createButton "Toggle Fixed" false (fun () ->
        state <- state.SelectedNodes |> Set.fold (fun s id -> toggleFixed id s) state
        refresh ())
    let composeBtn = createButton "Compose" false (fun () ->
        if state.SelectedNodes.Count > 0 then
            let hasInput =
                state.SelectedNodes
                |> Set.exists (fun id ->
                    match Map.tryFind id state.Nodes with
                    | Some n -> n.GateType = Input
                    | None -> false)
            let hasOutput =
                state.SelectedNodes
                |> Set.exists (fun id ->
                    match Map.tryFind id state.Nodes with
                    | Some n -> n.GateType = Output
                    | None -> false)
            if hasInput && hasOutput then
                let name =
                    match window.prompt ("Name your custom gate:", "Custom") with
                    | null -> "Custom"
                    | s when s.Trim() = "" -> "Custom"
                    | s -> s
                state <- composeSelection name state
                state.SelectedNodes
                |> Set.toSeq
                |> Seq.tryHead
                |> Option.bind (fun id -> Map.tryFind id state.Nodes)
                |> Option.iter (fun n ->
                    match n.GateType with
                    | Custom def ->
                        if customGates |> Seq.exists (fun g -> g.Name = def.Name) |> not then
                            customGates.Add def
                    | _ -> ())
                refresh ())
    toolbar.appendChild clearBtn |> ignore
    toolbar.appendChild deleteBtn |> ignore
    toolbar.appendChild renameBtn |> ignore
    toolbar.appendChild fixedBtn |> ignore
    toolbar.appendChild composeBtn |> ignore

    let spacer2 = document.createElement ("span")
    setStyles spacer2 "width:16px;display:inline-block;"
    toolbar.appendChild spacer2 |> ignore

    let physicsBtn =
        let label = if state.PhysicsPaused then "Resume Physics" else "Pause Physics"
        createButton label false (fun () -> state <- togglePhysics state; refresh ())
    toolbar.appendChild physicsBtn |> ignore

    status.innerText <-
        match state.Mode with
        | Select -> "Click to select/drag. Shift+click to multi-select. Drag empty space for box select."
        | AddAnd -> "Click on the canvas to add AND gates."
        | AddOr -> "Click on the canvas to add OR gates."
        | AddNand -> "Click on the canvas to add NAND gates."
        | AddInput -> "Click to add Input ports (blue) for custom gates."
        | AddOutput -> "Click to add Output ports (orange) for custom gates."
        | AddEdge -> "Drag from a gate/button output to a gate input."
        | AddCustom def ->
            if isForbiddenDef def then
                sprintf "Cannot place %s inside itself or one of its descendants." def.Name
            else
                sprintf "Click to place %s gates." def.Name
    toolbar.appendChild status |> ignore

let createCanvas () =
    let canvas = document.createElement ("canvas") :?> HTMLCanvasElement
    canvas.width <- int state.CanvasWidth
    canvas.height <- int state.CanvasHeight
    setStyles canvas "display:block;cursor:default;"
    canvas

let getMousePos (canvas: HTMLCanvasElement) (ev: MouseEvent) =
    let rect = canvas.getBoundingClientRect ()
    (ev.clientX - rect.left, ev.clientY - rect.top)

let init () =
    let toolbar = createToolbar ()
    let status = createStatus ()
    document.body.appendChild toolbar |> ignore

    let canvas = createCanvas ()
    document.body.appendChild canvas |> ignore

    let ctx = canvas.getContext_2d ()

    let applyResize () =
        let width = window.innerWidth - 16.0
        let height = window.innerHeight - 80.0
        canvas.width <- int width
        canvas.height <- int height
        state <- resize width height state

    canvas.onmousemove <- fun ev ->
        let (x, y) = getMousePos canvas ev
        state <- handleMouseMove x y state

    canvas.onmousedown <- fun ev ->
        let (x, y) = getMousePos canvas ev
        match state.Mode with
        | AddCustom def when isForbiddenDef def ->
            () // prevent recursive self-placement
        | _ ->
            state <- handleMouseDown x y ev.shiftKey ev.ctrlKey state
            updateToolbar toolbar status

    canvas.ondblclick <- fun ev ->
        let (x, y) = getMousePos canvas ev
        match hitTestNode x y state with
        | Some id -> enterCustomGate toolbar status id
        | None -> ()

    canvas.onmouseup <- fun ev ->
        let (x, y) = getMousePos canvas ev
        state <- handleMouseUp x y state
        updateToolbar toolbar status

    canvas.onmouseleave <- fun _ ->
        state <- { state with Hovered = None; Drag = NoDrag }

    document.onkeydown <- fun ev ->
        if ev.key = "Delete" || ev.key = "Backspace" then
            state <- deleteSelected state
            updateToolbar toolbar status

    window.onresize <- fun _ -> applyResize ()

    applyResize ()
    updateToolbar toolbar status

    let rec loop (_: float) =
        let cursor =
            match state.Mode with
            | AddAnd | AddOr | AddNand | AddInput | AddOutput | AddEdge | AddCustom _ -> "crosshair"
            | Select ->
                match state.Drag with
                | SelectBox _ -> "crosshair"
                | _ ->
                    match state.Hovered with
                    | Some (Choice1Of2 _) -> "move"
                    | _ -> "default"
        setStyles canvas (sprintf "display:block;cursor:%s;" cursor)
        state <- physicsStep state
        render ctx state
        window.requestAnimationFrame loop |> ignore

    window.requestAnimationFrame loop |> ignore

init ()
