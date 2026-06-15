module GraphEditor

open System
open Browser.Types
open Types

let nodeRadius = 28.0
let hoverExtra = 4.0
let hitRadius = nodeRadius + hoverExtra
let clickThreshold = 4.0

let inline color (s: string) = Fable.Core.U3<_, _, _>.Case1 s


let gateLabel gateType =
    match gateType with
    | And -> "AND"
    | Or -> "OR"
    | Nand -> "NAND"
    | Input -> "IN"
    | Output -> "OUT"
    | Custom def -> def.Name


let isLogicGate gateType =
    match gateType with
    | And | Or | Nand | Custom _ -> true
    | _ -> false

let isCustom gateType =
    match gateType with
    | Custom _ -> true
    | _ -> false

// Large limits keep the simple gate model unrestricted while still leaving
// room for future per-gate port constraints.
let inputCount gateType =
    match gateType with
    | And | Or | Nand -> 100
    | Input -> 0
    | Output -> 100
    | Custom def -> List.length def.Inputs

let outputCount gateType =
    match gateType with
    | And | Or | Nand | Output -> 1
    | Input -> 100
    | Custom def -> List.length def.Outputs

let baseRadius gateType =
    match gateType with
    | Input | Output -> 18.0
    | Custom def ->
        let ports = max (List.length def.Inputs) (List.length def.Outputs)
        max nodeRadius (float ports * 9.0 + 12.0)
    | _ -> nodeRadius

let makeNode id gateType x y =
    let outs = Array.init (outputCount gateType) (fun _ -> false)
    {
        Id = id
        GateType = gateType
        X = x
        Y = y
        Vx = 0.0
        Vy = 0.0
        InputCount = inputCount gateType
        OutputCount = outputCount gateType
        Outputs = outs
        Label = gateLabel gateType
        Radius = baseRadius gateType
        Fixed = false
    }

let emptyState canvasWidth canvasHeight = {
    Nodes = Map.empty
    Edges = Map.empty
    NextNodeId = 0
    NextEdgeId = 0
    Mode = Select
    SelectedNodes = Set.empty
    SelectedEdges = Set.empty
    Hovered = None
    Drag = NoDrag
    PhysicsPaused = false
    CanvasWidth = canvasWidth
    CanvasHeight = canvasHeight
    MouseX = 0.0
    MouseY = 0.0
    CustomStates = Map.empty
}

let distPointToSegment px py x1 y1 x2 y2 =
    let dx = x2 - x1
    let dy = y2 - y1
    if dx = 0.0 && dy = 0.0 then
        sqrt ((px - x1) ** 2.0 + (py - y1) ** 2.0)
    else
        let len2 = dx * dx + dy * dy
        let t = max 0.0 (min 1.0 ((px - x1) * dx + (py - y1) * dy) / len2)
        let projX = x1 + t * dx
        let projY = y1 + t * dy
        sqrt ((px - projX) ** 2.0 + (py - projY) ** 2.0)

let hitTestNode (x: float) (y: float) (state: GraphState) =
    state.Nodes
    |> Map.tryPick (fun _ n ->
        let dx = x - n.X
        let dy = y - n.Y
        if dx * dx + dy * dy <= hitRadius * hitRadius then Some n.Id
        else None)

let hitTestEdge (x: float) (y: float) (state: GraphState) =
    state.Edges
    |> Map.tryPick (fun _ e ->
        match Map.tryFind e.Source state.Nodes, Map.tryFind e.Target state.Nodes with
        | Some s, Some t ->
            let d = distPointToSegment x y s.X s.Y t.X t.Y
            if d <= 8.0 then Some e.Id else None
        | _ -> None)

let getHit (x: float) (y: float) (state: GraphState) =
    match hitTestNode x y state with
    | Some id -> Some (Choice1Of2 id)
    | None ->
        match hitTestEdge x y state with
        | Some id -> Some (Choice2Of2 id)
        | None -> None

let nodesInRect (x1: float) (y1: float) (x2: float) (y2: float) (state: GraphState) =
    let left = min x1 x2
    let right = max x1 x2
    let top = min y1 y2
    let bottom = max y1 y2
    state.Nodes
    |> Map.toSeq
    |> Seq.choose (fun (_, n) ->
        if n.X >= left && n.X <= right && n.Y >= top && n.Y <= bottom
        then Some n.Id else None)
    |> Set.ofSeq

let addGate gateType x y (state: GraphState) =
    let id = state.NextNodeId
    let n = makeNode id gateType x y
    { state with
        Nodes = Map.add id n state.Nodes
        NextNodeId = id + 1
        SelectedNodes = Set.singleton id
        SelectedEdges = Set.empty }


let addAnd x y state = addGate And x y state
let addOr x y state = addGate Or x y state
let addNand x y state = addGate Nand x y state
let addInput x y state = addGate Input x y state
let addOutput x y state = addGate Output x y state

let firstFreeInputPort targetId (state: GraphState) =
    match Map.tryFind targetId state.Nodes with
    | Some target ->
        let used =
            state.Edges
            |> Map.toSeq
            |> Seq.choose (fun (_, e) -> if e.Target = targetId then Some e.TargetPort else None)
            |> Set.ofSeq
        [0 .. target.InputCount - 1]
        |> List.tryFind (fun i -> not (Set.contains i used))
    | None -> None

let firstFreeOutputPort sourceId (state: GraphState) =
    match Map.tryFind sourceId state.Nodes with
    | Some source when isCustom source.GateType ->
        // Custom gates have a fixed number of unique outputs; each port can only
        // drive one edge.
        let used =
            state.Edges
            |> Map.toSeq
            |> Seq.choose (fun (_, e) -> if e.Source = sourceId then Some e.SourcePort else None)
            |> Set.ofSeq
        [0 .. source.OutputCount - 1]
        |> List.tryFind (fun i -> not (Set.contains i used))
    | Some _ -> Some 0
    | None -> None

let rec simulate (state: GraphState) =
    simulateWithForced Map.empty state

and simulateWithForced (forcedOutputs: Map<NodeId, bool array>) (state: GraphState) =
    // Persistent internal state for custom gates evaluated during this simulation.
    // It is captured in a ref so nested propagation can update it without threading
    // it through every helper.
    let customStates = ref state.CustomStates

    // adjacency: source node -> set of nodes that read its output
    let successors =
        state.Edges
        |> Map.fold (fun succ _ e ->
            let current = Map.tryFind e.Source succ |> Option.defaultValue Set.empty
            Map.add e.Source (Set.add e.Target current) succ) Map.empty

    // Collect the current values on every wire that flows into a given node.
    let inputsFor nodes nodeId =
        state.Edges
        |> Map.toSeq
        |> Seq.choose (fun (_, e) ->
            if e.Target <> nodeId then None
            else
                match Map.tryFind e.Source nodes with
                | Some source when e.SourcePort < source.Outputs.Length ->
                    Some source.Outputs.[e.SourcePort]
                | _ -> None)
        |> Seq.toList

    // Evaluate a custom gate by running its internal circuit with the current
    // input values forced onto its input-mapped internal nodes.
    let evaluateCustom nodes (n: Node) (def: CustomGateDef) =
        // Each external input port on the custom gate is driven by the wires that
        // target that specific port. Multiple wires to the same port are ORed.
        let inputValuesByPort =
            state.Edges
            |> Map.toSeq
            |> Seq.choose (fun (_, e) ->
                if e.Target <> n.Id then None
                else
                    match Map.tryFind e.Source nodes with
                    | Some source when e.SourcePort < source.Outputs.Length ->
                        Some (e.TargetPort, source.Outputs.[e.SourcePort])
                    | _ -> None)
            |> Seq.groupBy fst
            |> Seq.map (fun (port, pairs) -> port, pairs |> Seq.exists snd)
            |> Map.ofSeq

        let forced =
            def.Inputs
            |> List.mapi (fun idx (innerId, innerPort) ->
                let value = Map.tryFind idx inputValuesByPort |> Option.defaultValue false
                innerId, (innerPort, value))
            |> List.groupBy fst
            |> List.map (fun (innerId, pairs) ->
                // Build the forced output array for this inner node.
                match Map.tryFind innerId def.InternalNodes with
                | Some inner when inner.GateType = Input ->
                    // Input nodes fan out to many gates, each outgoing edge may use a
                    // different source port. All output ports must carry the same value.
                    let value =
                        match pairs with
                        | (_, (_, v)) :: _ -> v
                        | _ -> false
                    let outs = Array.create inner.OutputCount value
                    innerId, outs
                | Some inner ->
                    let outs = Array.copy inner.Outputs
                    pairs |> List.iter (fun (_, (port, value)) ->
                        if port < outs.Length then outs.[port] <- value)
                    innerId, outs
                | None -> innerId, [||])
            |> Map.ofList

        // Start from the persisted internal state so cyclic internal circuits
        // (latches / registers) remember their previous values. Fall back to the
        // composed snapshot the first time the gate is evaluated. Crucially, we
        // also keep the persisted CustomStates so nested custom gates retain their
        // own internal state (e.g. a memory cell inside a register).
        let startNodes, startCustomStates =
            match Map.tryFind n.Id customStates.Value with
            | Some prev -> prev.Nodes, prev.CustomStates
            | None -> def.InternalNodes, Map.empty

        let internalState =
            { state with
                Nodes = startNodes
                Edges = def.InternalEdges
                CustomStates = startCustomStates }
            |> simulateWithForced forced

        customStates.Value <- Map.add n.Id internalState customStates.Value

        def.Outputs
        |> List.mapi (fun idx (innerId, innerPort) ->
            if idx < n.OutputCount then
                match Map.tryFind innerId internalState.Nodes with
                | Some inner when innerPort < inner.Outputs.Length -> inner.Outputs.[innerPort]
                | _ -> false
            else false)
        |> Array.ofList


    // Compute the full output array for a node.
    let computeOutputs nodes (n: Node) =
        match Map.tryFind n.Id forcedOutputs with
        | Some outs -> outs
        | None ->
            match n.GateType with
            | Input -> n.Outputs
            | Output ->
                let ins = inputsFor nodes n.Id
                let value = not (List.isEmpty ins) && ins |> List.exists (fun x -> x)
                Array.create n.OutputCount value
            | And ->
                let ins = inputsFor nodes n.Id
                let value = not (List.isEmpty ins) && ins |> List.forall (fun x -> x)
                Array.create n.OutputCount value
            | Or ->
                let ins = inputsFor nodes n.Id
                let value = not (List.isEmpty ins) && ins |> List.exists (fun x -> x)
                Array.create n.OutputCount value
            | Nand ->
                let ins = inputsFor nodes n.Id
                let value = List.isEmpty ins || ins |> List.exists (fun x -> not x)
                Array.create n.OutputCount value
            | Custom def -> evaluateCustom nodes n def

    let outputsEqual (a: bool array) (b: bool array) =
        a.Length = b.Length && Array.forall2 (=) a b

    // Event-driven propagation: only re-evaluate nodes whose inputs may have changed,
    // and push their successors when any output changes. This is equivalent to a
    // topological evaluation for acyclic circuits and naturally handles deep chains.
    let rec propagate (nodes: Map<NodeId, Node>) (queue: Set<NodeId>) (safety: int) =
        if Set.isEmpty queue || safety <= 0 then nodes
        else
            let nodeId = Set.minElement queue
            let rest = Set.remove nodeId queue
            match Map.tryFind nodeId nodes with
            | None -> propagate nodes rest safety
            | Some n ->
                let newOutputs = computeOutputs nodes n
                if outputsEqual n.Outputs newOutputs then
                    propagate nodes rest (safety - 1)
                else
                    let newNodes = Map.add nodeId { n with Outputs = newOutputs } nodes
                    let next = Map.tryFind nodeId successors |> Option.defaultValue Set.empty
                    propagate newNodes (Set.union rest next) (safety - 1)

    let allNodes = state.Nodes |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    // Deeply nested custom gates (especially sequential/memory cells) may need many
    // propagation steps to reach a stable state. Use a generous limit.
    let safety = max (Set.count allNodes * 100) 10000
    { state with
        Nodes = propagate state.Nodes allNodes safety
        CustomStates = customStates.Value }

let addEdgeWithPorts source sourcePort target targetPort (state: GraphState) =
    if source = target then state
    elif state.Edges |> Map.exists (fun _ e ->
        e.Source = source && e.SourcePort = sourcePort && e.Target = target && e.TargetPort = targetPort) then state
    else
        let id = state.NextEdgeId
        let e = { Id = id; Source = source; SourcePort = sourcePort; Target = target; TargetPort = targetPort; Label = None }
        { state with
            Edges = Map.add id e state.Edges
            NextEdgeId = id + 1
            SelectedNodes = Set.empty
            SelectedEdges = Set.singleton id
            Drag = NoDrag }
        |> simulate

let addEdge source target (state: GraphState) =
    if source = target then state
    else
        match firstFreeOutputPort source state, firstFreeInputPort target state with
        | Some sp, Some tp -> addEdgeWithPorts source sp target tp state
        | _ -> state

let toggleInput nodeId (state: GraphState) =
    match Map.tryFind nodeId state.Nodes with
    | Some n when n.GateType = Input ->
        let value = not (n.Outputs.Length > 0 && n.Outputs.[0])
        let newOutputs = Array.create n.OutputCount value
        let nodes = Map.add nodeId { n with Outputs = newOutputs } state.Nodes
        simulate { state with Nodes = nodes }
    | _ -> state

let toggleFixed nodeId (state: GraphState) =
    match Map.tryFind nodeId state.Nodes with
    | Some n ->
        let nodes = Map.add nodeId { n with Fixed = not n.Fixed } state.Nodes
        { state with Nodes = nodes }
    | None -> state

let deleteSelected (state: GraphState) =
    let edgesToRemove =
        state.Edges
        |> Map.filter (fun _ e ->
            Set.contains e.Source state.SelectedNodes ||
            Set.contains e.Target state.SelectedNodes ||
            Set.contains e.Id state.SelectedEdges)
        |> Map.toList
        |> List.map fst
    let nodesToRemove = state.SelectedNodes
    let newState =
        { state with
            Nodes = nodesToRemove |> Set.fold (fun m id -> Map.remove id m) state.Nodes
            Edges = edgesToRemove |> List.fold (fun m eid -> Map.remove eid m) state.Edges
            CustomStates = nodesToRemove |> Set.fold (fun m id -> Map.remove id m) state.CustomStates
            SelectedNodes = Set.empty
            SelectedEdges = Set.empty
            Hovered = None }
    simulate newState

let composeSelection name (state: GraphState) =
    if Set.isEmpty state.SelectedNodes then state
    else
        let internalNodes =
            state.Nodes
            |> Map.filter (fun id _ -> Set.contains id state.SelectedNodes)
        let inputNodes =
            internalNodes
            |> Map.toSeq
            |> Seq.filter (fun (_, n) -> n.GateType = Input)
            |> Seq.sortBy (fun (_, n) -> n.Y, n.X)
            |> Seq.map fst
            |> Seq.toList
        let outputNodes =
            internalNodes
            |> Map.toSeq
            |> Seq.filter (fun (_, n) -> n.GateType = Output)
            |> Seq.sortBy (fun (_, n) -> n.Y, n.X)
            |> Seq.map fst
            |> Seq.toList

        if List.isEmpty inputNodes || List.isEmpty outputNodes then state
        else
            let internalEdges =
                state.Edges
                |> Map.filter (fun _ e ->
                    Set.contains e.Source state.SelectedNodes &&
                    Set.contains e.Target state.SelectedNodes)
            let outputEdges =
                state.Edges
                |> Map.toSeq
                |> Seq.filter (fun (_, e) ->
                    List.contains e.Source outputNodes &&
                    not (Set.contains e.Target state.SelectedNodes))
                |> Seq.map snd
                |> Seq.toList

            let inputs = inputNodes |> List.map (fun id -> id, 0)
            let outputs = outputNodes |> List.map (fun id -> id, 0)

            let def = {
                Name = name
                Inputs = inputs
                Outputs = outputs
                InternalNodes = internalNodes
                InternalEdges = internalEdges
            }

            let (sumX, sumY, count) =
                internalNodes
                |> Map.fold (fun (sx, sy, c) _ n -> (sx + n.X, sy + n.Y, c + 1)) (0.0, 0.0, 0)
            let cx = sumX / float count
            let cy = sumY / float count

            let newId = state.NextNodeId
            let customNode = makeNode newId (Custom def) cx cy

            let nodes =
                state.SelectedNodes
                |> Set.fold (fun m id -> Map.remove id m) state.Nodes
                |> Map.add newId customNode

            let edges =
                state.Edges
                |> Map.filter (fun _ e ->
                    not (Set.contains e.Source state.SelectedNodes) &&
                    not (Set.contains e.Target state.SelectedNodes))

            let (edges, nextEdgeId) =
                outputEdges
                |> List.fold (fun (edges, nextId) e ->
                    match List.tryFindIndex (fun outId -> outId = e.Source) outputNodes with
                    | Some idx ->
                        let newEdge = {
                            Id = nextId
                            Source = newId
                            SourcePort = idx
                            Target = e.Target
                            TargetPort = e.TargetPort
                            Label = None }
                        Map.add nextId newEdge edges, nextId + 1
                    | None -> edges, nextId) (edges, state.NextEdgeId)

            let newState = {
                state with
                    Nodes = nodes
                    Edges = edges
                    NextNodeId = newId + 1
                    NextEdgeId = nextEdgeId
                    SelectedNodes = Set.singleton newId
                    SelectedEdges = Set.empty
            }
            simulate newState

let clearGraph (state: GraphState) =
    { state with
        Nodes = Map.empty
        Edges = Map.empty
        CustomStates = Map.empty
        NextNodeId = 0
        NextEdgeId = 0
        SelectedNodes = Set.empty
        SelectedEdges = Set.empty
        Hovered = None
        Drag = NoDrag }

let setMode mode (state: GraphState) =
    { state with
        Mode = mode
        Drag =
            match mode with
            | Select -> state.Drag
            | _ -> NoDrag }

let togglePhysics (state: GraphState) =
    { state with PhysicsPaused = not state.PhysicsPaused }

let resize width height (state: GraphState) =
    { state with CanvasWidth = width; CanvasHeight = height }

let physicsStep (state: GraphState) =
    if state.PhysicsPaused then state
    else
        let pinned =
            match state.Drag with
            | DragNode _ -> state.SelectedNodes
            | DragSelection _ -> state.SelectedNodes
            | EdgeDrag (id, _, _, _) -> Set.singleton id
            | _ -> Set.empty
        { state with Nodes = Physics.applyForces state pinned }

let drawGrid (ctx: CanvasRenderingContext2D) width height =
    ctx.save ()
    ctx.strokeStyle <- color "#e5e7eb"
    ctx.lineWidth <- 1.0
    let step = 40.0
    let mutable x = 0.0
    while x <= width do
        ctx.beginPath ()
        ctx.moveTo (x, 0.0)
        ctx.lineTo (x, height)
        ctx.stroke ()
        x <- x + step
    let mutable y = 0.0
    while y <= height do
        ctx.beginPath ()
        ctx.moveTo (0.0, y)
        ctx.lineTo (width, y)
        ctx.stroke ()
        y <- y + step
    ctx.restore ()

/// Point on a node's circle that faces the given (otherX, otherY) point.
let circleIntersection (n: Node) (otherX: float) (otherY: float) =
    let dx = otherX - n.X
    let dy = otherY - n.Y
    let dist = sqrt (dx * dx + dy * dy)
    if dist < 0.001 then
        (n.X + n.Radius, n.Y)
    else
        let dirX = dx / dist
        let dirY = dy / dist
        (n.X + dirX * n.Radius, n.Y + dirY * n.Radius)

/// Fixed angular position for an input ball on a custom gate.
let customInputPos (n: Node) (port: PortIndex) =
    match n.GateType with
    | Custom def ->
        let count = max 1 (List.length def.Inputs)
        let idx = float (port + 1)
        let angle = Math.PI / 2.0 + idx * Math.PI / float (count + 1)
        (n.X + Math.Cos angle * n.Radius, n.Y + Math.Sin angle * n.Radius)
    | _ -> circleIntersection n (n.X - 1.0) n.Y

/// Fixed angular position for an output ball on a custom gate.
let customOutputPos (n: Node) (port: PortIndex) =
    match n.GateType with
    | Custom def ->
        let count = max 1 (List.length def.Outputs)
        let idx = float (port + 1)
        let angle = -Math.PI / 2.0 + idx * Math.PI / float (count + 1)
        (n.X + Math.Cos angle * n.Radius, n.Y + Math.Sin angle * n.Radius)
    | _ -> circleIntersection n (n.X + 1.0) n.Y

let bubbleRadius = 8.0
let bubbleDistance = 18.0
let bubbleCharWidth = 5.5
let bubblePadding = 8.0

/// Half-width of a pill-shaped bubble for a given label. The height is always
/// 2 * bubbleRadius.
let pillHalfWidth (label: string) =
    max bubbleRadius ((float label.Length * bubbleCharWidth + bubblePadding) / 2.0)

/// Draw a pill-shaped bubble centered at (cx, cy).
let drawPill (ctx: CanvasRenderingContext2D) (cx: float) (cy: float) (halfWidth: float) (radius: float) =
    let r = radius
    let x1 = cx - halfWidth + r
    let x2 = cx + halfWidth - r
    let y1 = cy - r
    let y2 = cy + r
    ctx.beginPath ()
    ctx.moveTo (x1, y1)
    ctx.lineTo (x2, y1)
    ctx.arc (x2, cy, r, -Math.PI / 2.0, Math.PI / 2.0)
    ctx.lineTo (x1, y2)
    ctx.arc (x1, cy, r, Math.PI / 2.0, Math.PI * 1.5)
    ctx.closePath ()

/// Available output ports for a node (only custom gates have unique outputs).
let availableOutputPorts (state: GraphState) (nodeId: NodeId) =
    match Map.tryFind nodeId state.Nodes with
    | Some n when isCustom n.GateType ->
        let used =
            state.Edges
            |> Map.toSeq
            |> Seq.choose (fun (_, e) -> if e.Source = nodeId then Some e.SourcePort else None)
            |> Set.ofSeq
        [0 .. n.OutputCount - 1]
        |> List.filter (fun i -> not (Set.contains i used))
    | Some n -> [0 .. n.OutputCount - 1] |> List.filter (fun _ -> true)
    | None -> []

/// Available input ports for a node (only custom gates have unique inputs).
let availableInputPorts (state: GraphState) (nodeId: NodeId) =
    match Map.tryFind nodeId state.Nodes with
    | Some n when isCustom n.GateType ->
        let used =
            state.Edges
            |> Map.toSeq
            |> Seq.choose (fun (_, e) -> if e.Target = nodeId then Some e.TargetPort else None)
            |> Set.ofSeq
        [0 .. n.InputCount - 1]
        |> List.filter (fun i -> not (Set.contains i used))
    | Some n -> [0 .. n.InputCount - 1] |> List.filter (fun _ -> true)
    | None -> []

/// Positions of source output bubbles around a custom node.
let outputBubblePositions (n: Node) (ports: PortIndex list) =
    let count = List.length ports
    ports
    |> List.mapi (fun i port ->
        let idx = float (i + 1)
        let angle = -Math.PI / 2.0 + idx * Math.PI / float (count + 1)
        let r = n.Radius + bubbleDistance
        port, (n.X + Math.Cos angle * r, n.Y + Math.Sin angle * r))

/// Positions of target input bubbles around a custom node.
let inputBubblePositions (n: Node) (ports: PortIndex list) =
    let count = List.length ports
    ports
    |> List.mapi (fun i port ->
        let idx = float (i + 1)
        let angle = Math.PI / 2.0 + idx * Math.PI / float (count + 1)
        let r = n.Radius + bubbleDistance
        port, (n.X + Math.Cos angle * r, n.Y + Math.Sin angle * r))

let hitTestBubble (x: float) (y: float) (bubbles: (PortIndex * (float * float) * string) list) =
    bubbles
    |> List.tryPick (fun (port, (bx, by), label) ->
        let hw = pillHalfWidth label
        let hh = bubbleRadius
        if x >= bx - hw && x <= bx + hw && y >= by - hh && y <= by + hh then Some port else None)

/// Find a custom-gate input bubble under the cursor. Excludes the source node of
/// an in-progress wire so a gate's own bubbles are not treated as its target.
let findInputBubbleHit (x: float) (y: float) (excludeSourceId: NodeId) (state: GraphState) =
    state.Nodes
    |> Map.toSeq
    |> Seq.choose (fun (_, n) ->
        if n.Id = excludeSourceId then None
        else
            match n.GateType with
            | Custom def ->
                let ports = availableInputPorts state n.Id
                if List.length ports > 1 then
                    let positions = inputBubblePositions n ports
                    let bubbles =
                        positions
                        |> List.map (fun (port, (bx, by)) ->
                            let label =
                                if port < def.Inputs.Length then
                                    let (innerId, _) = def.Inputs.[port]
                                    match Map.tryFind innerId def.InternalNodes with
                                    | Some inner when inner.Label <> "" -> inner.Label
                                    | _ -> string port
                                else string port
                            port, (bx, by), label)
                    hitTestBubble x y bubbles |> Option.map (fun port -> n.Id, port)
                else None
            | _ -> None)
    |> Seq.tryHead

let drawArrowHead (ctx: CanvasRenderingContext2D) (x1: float) (y1: float) (x2: float) (y2: float) (stroke: string) =
    let angle = atan2 (y2 - y1) (x2 - x1)
    let size = 8.0
    ctx.save ()
    ctx.strokeStyle <- color stroke
    ctx.fillStyle <- color stroke
    ctx.lineWidth <- 1.0
    ctx.translate (x2, y2)
    ctx.rotate (angle)
    ctx.beginPath ()
    ctx.moveTo (0.0, 0.0)
    ctx.lineTo (-size, -size / 2.0)
    ctx.lineTo (-size, size / 2.0)
    ctx.closePath ()
    ctx.fill ()
    ctx.restore ()

let drawEdge (ctx: CanvasRenderingContext2D) (state: GraphState) (e: Edge) =
    match Map.tryFind e.Source state.Nodes, Map.tryFind e.Target state.Nodes with
    | Some s, Some t when e.SourcePort < s.OutputCount && e.TargetPort < t.InputCount ->
        let isSelected = Set.contains e.Id state.SelectedEdges
        let isHovered = state.Hovered = Some (Choice2Of2 e.Id)
        let signal = e.SourcePort < s.Outputs.Length && s.Outputs.[e.SourcePort]
        let stroke =
            if isSelected then "#2563eb"
            elif isHovered then "#3b82f6"
            elif signal then "#22c55e"
            else "#6b7280"

        // Start/end the wire at the circle boundary facing the connected node.
        // No fixed I/O locations: the endpoint depends on the relative positions
        // of the two connected nodes.
        let (x1, y1) = circleIntersection s t.X t.Y
        let (x2, y2) = circleIntersection t s.X s.Y

        ctx.save ()
        ctx.strokeStyle <- color stroke
        ctx.lineWidth <- if isSelected then 3.0 elif signal then 3.0 else 2.0
        ctx.beginPath ()
        ctx.moveTo (x1, y1)
        ctx.lineTo (x2, y2)
        ctx.stroke ()

        // small input port dot where the wire meets the target circle
        ctx.fillStyle <- color (if signal then "#22c55e" else "#6b7280")
        ctx.beginPath ()
        ctx.arc (x2, y2, 3.0, 0.0, Math.PI * 2.0)
        ctx.fill ()

        // arrowhead showing direction from source to target
        drawArrowHead ctx x1 y1 x2 y2 stroke
        ctx.restore ()
    | _ -> ()

let drawNode (ctx: CanvasRenderingContext2D) (state: GraphState) (n: Node) =
    let isSelected = Set.contains n.Id state.SelectedNodes
    let isHovered = state.Hovered = Some (Choice1Of2 n.Id)
    let isEdgeSource =
        match state.Drag with
        | EdgeDrag (id, _, _, _) when id = n.Id -> true
        | _ -> false
    let outputHigh = n.Outputs.Length > 0 && n.Outputs.[0]

    ctx.save ()
    // selection/hover ring
    if isSelected || isHovered || isEdgeSource then
        ctx.beginPath ()
        ctx.arc (n.X, n.Y, n.Radius + 4.0, 0.0, Math.PI * 2.0)
        ctx.fillStyle <-
            color (
                if isSelected then "#bfdbfe"
                elif isEdgeSource then "#fde68a"
                else "#e5e7eb")
        ctx.fill ()

    // node body
    ctx.beginPath ()
    ctx.arc (n.X, n.Y, n.Radius, 0.0, Math.PI * 2.0)

    let fillColor, strokeColor =
        match n.GateType with
        | Input -> (if outputHigh then "#86efac" else "#e5e7eb"), "#1d4ed8"
        | Output -> (if outputHigh then "#f97316" else "#e5e7eb"), (if outputHigh then "#c2410c" else "#9ca3af")
        | _ -> "#ffffff", "#374151"
    ctx.fillStyle <- color fillColor
    ctx.fill ()
    ctx.strokeStyle <-
        color (
            if outputHigh && isLogicGate n.GateType then "#22c55e"
            elif isSelected then "#2563eb"
            elif isHovered then "#3b82f6"
            else strokeColor)
    ctx.lineWidth <- if n.Fixed then 3.0 else 2.0
    ctx.stroke ()

    // output glow when high (for gates only)
    if outputHigh && isLogicGate n.GateType then
        ctx.beginPath ()
        ctx.arc (n.X, n.Y, n.Radius + 2.0, 0.0, Math.PI * 2.0)
        ctx.strokeStyle <- color "#86efac"
        ctx.lineWidth <- 2.0
        ctx.stroke ()

    // fixed anchor indicator
    if n.Fixed then
        ctx.fillStyle <- color "#111827"
        ctx.font <- "10px sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText ("⚓", n.X, n.Y + n.Radius - 8.0)


    // label
    ctx.fillStyle <- color (if n.GateType = Input || n.GateType = Output then "#ffffff" else "#111827")
    ctx.font <- "bold 12px sans-serif"
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.fillText (n.Label, n.X, n.Y)

    ctx.restore ()

let drawEdgePreview (ctx: CanvasRenderingContext2D) (state: GraphState) =
    match state.Drag with
    | EdgeDrag (sourceId, sourcePort, _, _) ->
        match Map.tryFind sourceId state.Nodes with
        | Some source ->
            ctx.save ()
            ctx.strokeStyle <- color "#9ca3af"
            ctx.lineWidth <- 2.0
            ctx.setLineDash [| 5.0; 5.0 |]
            let (x1, y1) = circleIntersection source state.MouseX state.MouseY
            ctx.beginPath ()
            ctx.moveTo (x1, y1)
            ctx.lineTo (state.MouseX, state.MouseY)
            ctx.stroke ()
            ctx.restore ()
        | None -> ()
    | _ -> ()

let drawBubbles (ctx: CanvasRenderingContext2D) (state: GraphState) =
    match state.Drag with
    | EdgeDrag (sourceId, sourcePort, targetId, targetPort) ->
        // Source output bubbles (only for custom gates with unique outputs)
        match Map.tryFind sourceId state.Nodes with
        | Some source when isCustom source.GateType ->
            let def =
                match source.GateType with
                | Custom d -> d
                | _ -> failwith "expected custom gate"
            let ports = availableOutputPorts state sourceId
            if List.length ports > 1 then
                let positions = outputBubblePositions source ports
                positions
                |> List.iter (fun (port, (bx, by)) ->
                    ctx.save ()
                    let label =
                        if port < def.Outputs.Length then
                            let (innerId, _) = def.Outputs.[port]
                            match Map.tryFind innerId def.InternalNodes with
                            | Some inner when inner.Label <> "" -> inner.Label
                            | _ -> string port
                        else string port
                    let hw = pillHalfWidth label
                    let isSelected = port = sourcePort
                    ctx.fillStyle <- color (if isSelected then "#f97316" else "rgba(249, 115, 22, 0.3)")
                    drawPill ctx bx by hw bubbleRadius
                    ctx.fill ()
                    ctx.strokeStyle <- color "#f97316"
                    ctx.lineWidth <- 1.5
                    ctx.stroke ()
                    ctx.fillStyle <- color "#ffffff"
                    ctx.font <- "9px sans-serif"
                    ctx.textAlign <- "center"
                    ctx.textBaseline <- "middle"
                    ctx.fillText (label, bx, by)
                    ctx.restore ())
        | _ -> ()

        // Target input bubbles (only when hovering a custom target)
        match targetId with
        | Some tid ->
            match Map.tryFind tid state.Nodes with
            | Some target when isCustom target.GateType ->
                let def =
                    match target.GateType with
                    | Custom d -> d
                    | _ -> failwith "expected custom gate"
                let ports = availableInputPorts state tid
                if List.length ports > 1 then
                    let positions = inputBubblePositions target ports
                    positions
                    |> List.iter (fun (port, (bx, by)) ->
                        ctx.save ()
                        let label =
                            if port < def.Inputs.Length then
                                let (innerId, _) = def.Inputs.[port]
                                match Map.tryFind innerId def.InternalNodes with
                                | Some inner when inner.Label <> "" -> inner.Label
                                | _ -> string port
                            else string port
                        let hw = pillHalfWidth label
                        let isSelected = Some port = targetPort
                        ctx.fillStyle <- color (if isSelected then "#3b82f6" else "rgba(59, 130, 246, 0.3)")
                        drawPill ctx bx by hw bubbleRadius
                        ctx.fill ()
                        ctx.strokeStyle <- color "#3b82f6"
                        ctx.lineWidth <- 1.5
                        ctx.stroke ()
                        ctx.fillStyle <- color "#ffffff"
                        ctx.font <- "9px sans-serif"
                        ctx.textAlign <- "center"
                        ctx.textBaseline <- "middle"
                        ctx.fillText (label, bx, by)
                        ctx.restore ())
            | _ -> ()
        | None -> ()
    | _ -> ()

let drawSelectionBox (ctx: CanvasRenderingContext2D) (state: GraphState) =
    match state.Drag with
    | SelectBox (startX, startY) ->
        let x = min startX state.MouseX
        let y = min startY state.MouseY
        let w = abs (state.MouseX - startX)
        let h = abs (state.MouseY - startY)
        ctx.save ()
        ctx.strokeStyle <- color "#2563eb"
        ctx.fillStyle <- color "rgba(37, 99, 235, 0.15)"
        ctx.lineWidth <- 1.0
        ctx.setLineDash [| 4.0; 4.0 |]
        ctx.fillRect (x, y, w, h)
        ctx.strokeRect (x, y, w, h)
        ctx.restore ()
    | _ -> ()

let render (ctx: CanvasRenderingContext2D) (state: GraphState) =
    ctx.clearRect (0.0, 0.0, state.CanvasWidth, state.CanvasHeight)
    drawGrid ctx state.CanvasWidth state.CanvasHeight

    state.Edges |> Map.iter (fun _ e -> drawEdge ctx state e)
    drawEdgePreview ctx state
    state.Nodes |> Map.iter (fun _ n -> drawNode ctx state n)
    drawBubbles ctx state
    drawSelectionBox ctx state

let moveSelectedNodes (dx: float) (dy: float) (state: GraphState) =
    let nodes =
        state.Nodes
        |> Map.map (fun id n ->
            if Set.contains id state.SelectedNodes then
                { n with X = n.X + dx; Y = n.Y + dy }
            else n)
    { state with Nodes = nodes }


let handleMouseMove (x: float) (y: float) (state: GraphState) =
    let state = { state with MouseX = x; MouseY = y }
    match state.Drag with
    | DragNode (id, offX, offY, _, _) ->
        let newX = x - offX
        let newY = y - offY
        let dx = newX - (Map.find id state.Nodes).X
        let dy = newY - (Map.find id state.Nodes).Y
        let nodes =
            state.Nodes
            |> Map.map (fun i n ->
                if Set.contains i state.SelectedNodes then
                    { n with X = n.X + dx; Y = n.Y + dy }
                else n)
        { state with Nodes = nodes }
    | DragSelection (offX, offY, _, _) ->
        let dx = x - offX
        let dy = y - offY
        let nodes =
            state.Nodes
            |> Map.map (fun i n ->
                if Set.contains i state.SelectedNodes then
                    { n with X = n.X + dx; Y = n.Y + dy }
                else n)
        { state with Drag = DragSelection (x, y, 0.0, 0.0); Nodes = nodes }
    | EdgeDrag (sourceId, sourcePort, _, _) ->
        // Check if hovering a source output bubble on a custom gate.
        let newSourcePort =
            match Map.tryFind sourceId state.Nodes with
            | Some source when isCustom source.GateType ->
                let def =
                    match source.GateType with
                    | Custom d -> d
                    | _ -> failwith "expected custom gate"
                let ports = availableOutputPorts state sourceId
                if List.length ports > 1 then
                    let positions = outputBubblePositions source ports
                    let bubbles =
                        positions
                        |> List.map (fun (port, (bx, by)) ->
                            let label =
                                if port < def.Outputs.Length then
                                    let (innerId, _) = def.Outputs.[port]
                                    match Map.tryFind innerId def.InternalNodes with
                                    | Some inner when inner.Label <> "" -> inner.Label
                                    | _ -> string port
                                else string port
                            port, (bx, by), label)
                    hitTestBubble x y bubbles |> Option.defaultValue sourcePort
                elif List.length ports = 1 then
                    List.head ports
                else
                    sourcePort
            | _ -> sourcePort
        // Determine candidate target. Bubbles live outside the node circle, so
        // they must be detected explicitly; otherwise moving from the node body
        // to a bubble makes the bubble (and target) disappear.
        let hit = hitTestNode x y state
        let (targetId, targetPort) =
            match findInputBubbleHit x y sourceId state with
            | Some (tid, port) -> Some tid, Some port
            | None ->
                match hit with
                | Some tid when tid <> sourceId ->
                    match Map.tryFind tid state.Nodes with
                    | Some target when isCustom target.GateType ->
                        let def =
                            match target.GateType with
                            | Custom d -> d
                            | _ -> failwith "expected custom gate"
                        let ports = availableInputPorts state tid
                        if List.length ports > 1 then
                            let positions = inputBubblePositions target ports
                            let bubbles =
                                positions
                                |> List.map (fun (port, (bx, by)) ->
                                    let label =
                                        if port < def.Inputs.Length then
                                            let (innerId, _) = def.Inputs.[port]
                                            match Map.tryFind innerId def.InternalNodes with
                                            | Some inner when inner.Label <> "" -> inner.Label
                                            | _ -> string port
                                        else string port
                                    port, (bx, by), label)
                            let tp = hitTestBubble x y bubbles
                            Some tid, tp
                        elif List.length ports = 1 then
                            Some tid, Some (List.head ports)
                        else
                            None, None
                    | Some _ -> Some tid, None
                    | None -> None, None
                | _ -> None, None
        let hovered =
            targetId
            |> Option.map Choice1Of2
            |> Option.orElse (hit |> Option.map Choice1Of2)
        { state with Drag = EdgeDrag (sourceId, newSourcePort, targetId, targetPort); Hovered = hovered }
    | _ ->
        let hit = getHit x y state
        { state with Hovered = hit }


let handleMouseDown (x: float) (y: float) (shift: bool) (ctrl: bool) (state: GraphState) =
    let hit = getHit x y state
    match state.Mode with
    | AddAnd -> addAnd x y state
    | AddOr -> addOr x y state
    | AddNand -> addNand x y state
    | AddInput -> addInput x y state
    | AddOutput -> addOutput x y state
    | AddCustom def -> addGate (Custom def) x y state
    | AddEdge ->
        match hit with
        | Some (Choice1Of2 id) ->
            let sourcePort = firstFreeOutputPort id state |> Option.defaultValue 0
            { state with Drag = EdgeDrag (id, sourcePort, None, None); SelectedNodes = Set.singleton id; SelectedEdges = Set.empty }
        | _ -> state
    | Select ->
        match hit with
        | Some (Choice1Of2 id) ->
            let n = Map.find id state.Nodes
            let offX = x - n.X
            let offY = y - n.Y
            let newSelected =
                if shift || ctrl then
                    if Set.contains id state.SelectedNodes then
                        Set.remove id state.SelectedNodes
                    else
                        Set.add id state.SelectedNodes
                elif Set.contains id state.SelectedNodes then
                    state.SelectedNodes
                else
                    Set.singleton id
            { state with
                SelectedNodes = newSelected
                SelectedEdges = Set.empty
                Drag = DragNode (id, offX, offY, x, y) }
        | Some (Choice2Of2 id) ->
            let newSelected =
                if shift || ctrl then
                    if Set.contains id state.SelectedEdges then
                        Set.remove id state.SelectedEdges
                    else
                        Set.add id state.SelectedEdges
                else
                    Set.singleton id
            { state with
                SelectedEdges = newSelected
                SelectedNodes = Set.empty
                Drag = NoDrag }
        | None ->
            { state with
                SelectedNodes = Set.empty
                SelectedEdges = Set.empty
                Drag = SelectBox (x, y) }


let handleMouseUp (x: float) (y: float) (state: GraphState) =
    match state.Drag with
    | EdgeDrag (sourceId, sourcePort, targetId, targetPort) ->
        match targetId with
        | Some tid when tid <> sourceId ->
            let tp = targetPort |> Option.defaultWith (fun () -> firstFreeInputPort tid state |> Option.defaultValue 0)
            // Enforce one-edge-per-port on custom gates. Built-in gates remain unlimited.
            let sourceIsCustom =
                match Map.tryFind sourceId state.Nodes with
                | Some n -> isCustom n.GateType
                | None -> false
            let targetIsCustom =
                match Map.tryFind tid state.Nodes with
                | Some n -> isCustom n.GateType
                | None -> false
            let sourcePortUsed =
                sourceIsCustom &&
                state.Edges |> Map.exists (fun _ e -> e.Source = sourceId && e.SourcePort = sourcePort)
            let targetPortUsed =
                targetIsCustom &&
                state.Edges |> Map.exists (fun _ e -> e.Target = tid && e.TargetPort = tp)
            let state = { state with Drag = NoDrag }
            if not sourcePortUsed && not targetPortUsed then
                addEdgeWithPorts sourceId sourcePort tid tp state
            else
                state
        | _ ->
            { state with Drag = NoDrag }
    | DragNode (id, _, _, origX, origY) ->
        let moved = sqrt ((x - origX) ** 2.0 + (y - origY) ** 2.0)
        let state = { state with Drag = NoDrag }
        if moved < clickThreshold then
            match Map.tryFind id state.Nodes with
            | Some n when n.GateType = Input -> toggleInput id state
            | _ -> state
        else
            state
    | DragSelection _ ->
        { state with Drag = NoDrag }
    | SelectBox (startX, startY) ->
        let selected = nodesInRect startX startY x y state
        { state with
            Drag = NoDrag
            SelectedNodes = selected
            SelectedEdges = Set.empty }
    | _ -> state
