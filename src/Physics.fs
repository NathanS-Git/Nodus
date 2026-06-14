module Physics

open System
open Types

// Tuned for a pleasant, bouncy feel on a 1:1 pixel canvas.
let repulsionStrength = 8000.0
let springLength = 140.0
let springStrength = 0.04
let damping = 0.92
let centeringStrength = 0.003
let maxSpeed = 30.0
let timeStep = 0.5

let private clamp (value: float) (minVal: float) (maxVal: float) =
    max minVal (min maxVal value)

let private distanceAndDir x1 y1 x2 y2 =
    let dx = x2 - x1
    let dy = y2 - y1
    let dist = sqrt (dx * dx + dy * dy)
    if dist < 0.001 then
        (0.001, 1.0, 0.0)
    else
        (dist, dx / dist, dy / dist)

let applyForces (state: GraphState) (pinnedNodeIds: Set<NodeId>) =
    let centerX = state.CanvasWidth / 2.0
    let centerY = state.CanvasHeight / 2.0

    // Start with zero net force per node.
    let forces =
        state.Nodes
        |> Map.map (fun _ _ -> (0.0, 0.0))

    // Repulsion between every pair of nodes.
    let nodesList = state.Nodes |> Map.toList |> List.map snd
    let rec repulse pairs forces =
        match pairs with
        | [] -> forces
        | (a, b) :: rest ->
            let (dist, dirX, dirY) = distanceAndDir a.X a.Y b.X b.Y
            // Softened inverse-square repulsion so nearby nodes push hard but
            // distant ones still feel a gentle nudge.
            let force = repulsionStrength / (dist * dist + 1.0)
            let fx = dirX * force
            let fy = dirY * force
            let forces' =
                forces
                |> Map.add a.Id (fst (Map.find a.Id forces) - fx, snd (Map.find a.Id forces) - fy)
                |> Map.add b.Id (fst (Map.find b.Id forces) + fx, snd (Map.find b.Id forces) + fy)
            repulse rest forces'

    let pairs =
        [ for i in 0 .. nodesList.Length - 1 do
            for j in i + 1 .. nodesList.Length - 1 do
                yield (nodesList.[i], nodesList.[j]) ]
    let forces = repulse pairs forces

    // Spring attraction along edges.
    let forces =
        state.Edges
        |> Map.fold (fun forces _ e ->
            match Map.tryFind e.Source state.Nodes, Map.tryFind e.Target state.Nodes with
            | Some a, Some b ->
                let (dist, dirX, dirY) = distanceAndDir a.X a.Y b.X b.Y
                let displacement = dist - springLength
                let force = springStrength * displacement
                let fx = dirX * force
                let fy = dirY * force
                forces
                |> Map.add a.Id (fst (Map.find a.Id forces) + fx, snd (Map.find a.Id forces) + fy)
                |> Map.add b.Id (fst (Map.find b.Id forces) - fx, snd (Map.find b.Id forces) - fy)
            | _ -> forces) forces

    // Gentle pull toward the canvas center so the graph does not drift away.
    let forces =
        state.Nodes
        |> Map.fold (fun forces id n ->
            let fx = (centerX - n.X) * centeringStrength
            let fy = (centerY - n.Y) * centeringStrength
            Map.add id (fst (Map.find id forces) + fx, snd (Map.find id forces) + fy) forces) forces

    // Integrate velocities and positions, but keep fixed/pinned nodes anchored.
    state.Nodes
    |> Map.map (fun id n ->
        if n.Fixed || Set.contains id pinnedNodeIds then
            { n with Vx = 0.0; Vy = 0.0 }
        else
            let (fx, fy) = Map.find id forces
            let vx = clamp ((n.Vx + fx * timeStep) * damping) (-maxSpeed) maxSpeed
            let vy = clamp ((n.Vy + fy * timeStep) * damping) (-maxSpeed) maxSpeed
            { n with
                Vx = vx
                Vy = vy
                X = n.X + vx * timeStep
                Y = n.Y + vy * timeStep })
