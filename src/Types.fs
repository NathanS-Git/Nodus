module Types

type NodeId = int
type EdgeId = int
type PortIndex = int

type CustomGateDef = {
    Name: string
    mutable Inputs: (NodeId * PortIndex) list
    mutable Outputs: (NodeId * PortIndex) list
    mutable InternalNodes: Map<NodeId, Node>
    mutable InternalEdges: Map<EdgeId, Edge>
}

and GateType =
    | And
    | Or
    | Nand
    | Button
    | Input
    | Output
    | Custom of CustomGateDef

and Node = {
    Id: NodeId
    GateType: GateType
    X: float
    Y: float
    Vx: float
    Vy: float
    InputCount: int
    OutputCount: int
    Outputs: bool array
    Label: string
    Radius: float
    Fixed: bool
}

and Edge = {
    Id: EdgeId
    Source: NodeId
    SourcePort: PortIndex
    Target: NodeId
    TargetPort: PortIndex
    Label: string option
}

type ToolMode =
    | Select
    | AddEdge
    | AddAnd
    | AddOr
    | AddNand
    | AddButton
    | AddInput
    | AddOutput
    | AddCustom of CustomGateDef

type DragState =
    | NoDrag
    | DragNode of NodeId * offsetX: float * offsetY: float * origMouseX: float * origMouseY: float
    | DragSelection of offsetX: float * offsetY: float * origMouseX: float * origMouseY: float
    | EdgeDrag of sourceId: NodeId * sourcePort: PortIndex * targetId: NodeId option * targetPort: PortIndex option
    | SelectBox of startX: float * startY: float

type GraphState = {
    Nodes: Map<NodeId, Node>
    Edges: Map<EdgeId, Edge>
    NextNodeId: NodeId
    NextEdgeId: EdgeId
    Mode: ToolMode
    SelectedNodes: Set<NodeId>
    SelectedEdges: Set<EdgeId>
    Hovered: Choice<NodeId, EdgeId> option
    Drag: DragState
    PhysicsPaused: bool
    CanvasWidth: float
    CanvasHeight: float
    MouseX: float
    MouseY: float
    /// Persistent internal simulation state for each custom-gate instance.
    /// This lets cyclic internal circuits (latches/registers) retain their state
    /// across evaluations instead of restarting from the composed snapshot.
    CustomStates: Map<NodeId, GraphState>
}
