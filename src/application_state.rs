use crate::position::*;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use gtk::RadioButton;
use gtk::prelude::ToggleButtonExt;
use gtk::{
    prelude::{GtkMenuExt, GtkMenuItemExt, WidgetExt, WidgetExtManual},
    gdk::{EventMotion, EventButton, EventMask, Rectangle as GdkRectangle},
    cairo::Context,
    DrawingArea,
    Inhibit,
    Menu,
    MenuItem,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Handle {
    Symmetric(Offset, Position),
    Asymmetric(Offset, Position, Offset),
}

impl Handle {
    fn position(&self) -> Position {
        match self {
            Handle::Symmetric(_, p) => *p,
            Handle::Asymmetric(_, p, _) => *p,
        }
    }
    fn in_offset(&self) -> Offset {
        match self {
            Handle::Symmetric(o, _) => *o,
            Handle::Asymmetric(o, _, _) => *o,
        }
    }
    fn out_offset(&self) -> Offset {
        match self {
            Handle::Symmetric(o, _) => -o,
            Handle::Asymmetric(_, _, o) => *o,
        }
    }
}

#[derive(Debug)]
pub struct BezierEdge {
    pub from_offset: Offset,
    pub mid_handles: Vec<Handle>,
    pub to_offset: Offset,
}

#[derive(Debug)]
pub enum EdgeKind {
    Bezier(BezierEdge),
    Linear,
}

#[derive(Debug)]
pub struct Edge { // TODO: make EdgeKind and pull label into struct Edge { label, kind }
    pub label: String,
    pub kind: EdgeKind,
}

impl Edge {
    fn as_bezier_edge(&self) -> Option<&BezierEdge> {
        match self { Edge{ kind: EdgeKind::Bezier(e), ..} => Some(e), _ => None }
    }
    fn as_bezier_edge_mut(&mut self) -> Option<&mut BezierEdge> {
        match self { Edge{ kind: EdgeKind::Bezier(e), .. } => Some(e), _ => None }
    }
}

#[derive(Debug)]
pub struct Node {
    pub label: String,
    pub position: Position,
}

#[derive(Debug, Clone, Copy)]
enum ManipulableItem {
    /// Moving a node
    Node { idx: usize },
    /// Moving the position of a midpoint of a Bezier edge
    EdgeMiddlePosition { from_node: usize, to_node: usize, idx: usize },
    /// Moving the handle of a midpoint of a Bezier edge (out_handle means the second handle for asymmetric, or negative first handle for symmetric)
    EdgeMiddleHandle { from_node: usize, to_node: usize, idx: usize, out_handle: bool },
    /// Moving the handle of an endpoint of a Bezier edge
    EdgeHandle { from_node: usize, to_node: usize, from_handle: bool },
    /// Moving or relabeling a node label
    NodeLabel { idx: usize },
    /// Moving or relabeling an edge label
    EdgeLabel { from_node: usize, to_node: usize },
}

// TODO: Maybe make ManipulableItem have a method .actions() that lists the actions that can be performed?
pub struct Action {
    description: String,
    action: Box<dyn Fn(&mut ApplicationState)>,
}

#[derive(Debug, Clone, Copy)]
pub enum ManipulateError {
    InvalidNode { idx: usize },
    InvalidEdge { from_node: usize, to_node: usize },
    InvalidEdgeHandle { from_node: usize, to_node: usize, idx: usize },
    EdgeNotBezier { from_node: usize, to_node: usize },
    ItemIsNotLabel,
    ItemIsNotEdgeHandle,
}

impl ManipulableItem {
    pub fn is_movable(&self) -> bool {
        use ManipulableItem::*;
        match self {
            Node { .. } => true,
            EdgeMiddlePosition { .. } => true,
            EdgeMiddleHandle { .. } => true,
            EdgeHandle { .. } => true,
            NodeLabel { .. } => true,
            EdgeLabel { .. } => true,
        }
    }

    pub fn move_item(&self, state: &mut ApplicationState, position: Position) -> Result<(), ManipulateError> {
        use ManipulateError::*;
        match *self {
            ManipulableItem::Node { idx } => {
                state.nodes.get_mut(&idx).ok_or(InvalidNode{idx})?.position = position;
            },
            ManipulableItem::EdgeHandle { from_node, to_node, from_handle } => {
                let edge = 
                state.edges
                    .get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge{from_node, to_node})?
                    .as_bezier_edge_mut()
                    .ok_or(EdgeNotBezier{from_node, to_node})?;
                let from_node = &state.nodes[&from_node];
                let to_node = &state.nodes[&to_node];
                if from_handle {
                    // Move the from_handle
                    let new_from_offset = position - from_node.position;
                    edge.from_offset = new_from_offset;
                } else {
                    // Move the to_handle
                    let new_to_offset = position - to_node.position;
                    edge.to_offset = new_to_offset;
                }
            },
            ManipulableItem::EdgeMiddleHandle { from_node, to_node, idx, out_handle: is_out_handle } => {
                let edge = state.edges.get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge{from_node, to_node})?
                    .as_bezier_edge_mut()
                    .ok_or(EdgeNotBezier{from_node, to_node})?;
                let handle = &mut edge.mid_handles[idx];
                match handle {
                    Handle::Symmetric(in_offset, handle_position) => {
                        let new_offset = if !is_out_handle {position - *handle_position} else {*handle_position - position};
                        *in_offset = new_offset;
                    },
                    Handle::Asymmetric(in_offset, handle_position, out_offset) => {
                        if is_out_handle {
                            *out_offset = position - *handle_position;
                        } else {
                            *in_offset = position - *handle_position;
                        }
                    },
                };
            },
            ManipulableItem::EdgeMiddlePosition { from_node, to_node, idx } => {
                let edge = state.edges.get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge{from_node, to_node})?
                    .as_bezier_edge_mut()
                    .ok_or(EdgeNotBezier{from_node, to_node})?;
                let handle = &mut edge.mid_handles[idx];
                match handle {
                    Handle::Symmetric(_, handle_position) => {
                        *handle_position = position;
                    },
                    Handle::Asymmetric(_, handle_position, _) => {
                        *handle_position = position;
                    },
                };
            },
            ManipulableItem::NodeLabel{ idx } => {
                todo!("implement node label positioning with offsets")
            },
            ManipulableItem::EdgeLabel{ from_node, to_node } => {
                todo!("implement edge label positioning with offsets")
            },
        };
        Ok(())
    }

    pub fn is_label(&self) -> bool {
        use ManipulableItem::*;
        match self {
            NodeLabel { .. } => true,
            EdgeLabel { .. } => true,
            _ => false,
        }
    }

    pub fn replace_label(&self, state: &mut ApplicationState, new_label: String) -> Result<String, ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        match *self {
            // TODO: decide if Node should be able to be interpreted as NodeLabel when unambiguous
            Node { idx } | NodeLabel { idx } => {
                let node = state.nodes.get_mut(&idx)
                    .ok_or(InvalidNode{idx})?;
                Ok(std::mem::replace(&mut node.label, new_label))
            },
            EdgeLabel { from_node, to_node } => {
                let edge = state.edges.get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge{from_node, to_node})?;
                Ok(std::mem::replace(&mut edge.label, new_label))
            },
            _ => Err(ItemIsNotLabel)
        }
    }

    pub fn get_label<'a>(&self, state: &'a ApplicationState, new_label: String) -> Result<&'a str, ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        match *self {
            NodeLabel { idx } => {
                let node = state.nodes.get(&idx)
                    .ok_or(InvalidNode{idx})?;
                Ok(&node.label)
            },
            EdgeLabel { from_node, to_node } => {
                let edge = state.edges.get(&(from_node, to_node))
                    .ok_or(InvalidEdge{from_node, to_node})?;
                Ok(&edge.label)
            },
            _ => Err(ItemIsNotLabel)
        }
    }

    pub fn make_symmetric(&self, state: &mut ApplicationState) -> Result<(), ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        if let EdgeMiddleHandle { from_node, to_node, idx, .. }
             | EdgeMiddlePosition { from_node, to_node, idx } = *self
        {
            let edge = state.edges.get_mut(&(from_node, to_node))
                .ok_or(InvalidEdge {from_node, to_node})?;
            let edge = edge.as_bezier_edge_mut()
                .ok_or(EdgeNotBezier{from_node, to_node})?;
            let handle = edge.mid_handles.get_mut(idx)
                .ok_or(InvalidEdgeHandle { from_node, to_node, idx })?;
            match *handle {
                Handle::Symmetric(_, _) => {}, // Already symmetric
                Handle::Asymmetric(in_offset, position, out_offset) => {
                    let new_in_offset = (in_offset - out_offset) / 2.0;
                    *handle = Handle::Symmetric(new_in_offset, position);
                },
            };
            Ok(())
        } else {
            Err(ItemIsNotEdgeHandle)
        }
    }
    pub fn make_asymmetric(&self, state: &mut ApplicationState) -> Result<(), ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        if let EdgeMiddleHandle { from_node, to_node, idx, .. }
             | EdgeMiddlePosition { from_node, to_node, idx } = *self
        {
            let edge = state.edges.get_mut(&(from_node, to_node))
                .ok_or(InvalidEdge {from_node, to_node})?;
            let edge = edge.as_bezier_edge_mut()
                .ok_or(EdgeNotBezier{from_node, to_node})?;
            let handle = edge.mid_handles.get_mut(idx)
                .ok_or(InvalidEdgeHandle { from_node, to_node, idx })?;
            match *handle {
                Handle::Asymmetric(_, _, _) => {}, // Already asymmetric
                Handle::Symmetric(in_offset, position) => {
                    *handle = Handle::Asymmetric(in_offset, position, -in_offset);
                },
            };
            Ok(())
        } else {
            Err(ItemIsNotEdgeHandle)
        }
    }

    pub fn remove_item(&self, state: &mut ApplicationState) -> Result<(), ManipulateError> {
        use ManipulateError::*;
        match *self {
            ManipulableItem::Node { idx } => {
                state.remove_node(idx);
            },
            ManipulableItem::EdgeHandle { from_node, to_node, from_handle } => {
                // let edge = 
                // state.edges
                //     .get_mut(&(from_node, to_node))
                //     .ok_or(InvalidEdge{from_node, to_node})?
                //     .as_bezier_edge_mut()
                //     .ok_or(EdgeNotBezier{from_node, to_node})?;
                // let from_node = &state.nodes[&from_node];
                // let to_node = &state.nodes[&to_node];
                // if from_handle {
                //     // Move the from_handle
                //     let new_from_offset = position - from_node.position;
                //     edge.from_offset = new_from_offset;
                // } else {
                //     // Move the to_handle
                //     let new_to_offset = position - to_node.position;
                //     edge.to_offset = new_to_offset;
                // }
                todo!()
            },
            ManipulableItem::EdgeMiddleHandle { from_node, to_node, idx, out_handle: is_out_handle } => {
                // let edge = state.edges.get_mut(&(from_node, to_node))
                //     .ok_or(InvalidEdge{from_node, to_node})?
                //     .as_bezier_edge_mut()
                //     .ok_or(EdgeNotBezier{from_node, to_node})?;
                // let handle = &mut edge.mid_handles[idx];
                // match handle {
                //     Handle::Symmetric(in_offset, handle_position) => {
                //         let new_offset = if !is_out_handle {position - *handle_position} else {*handle_position - position};
                //         *in_offset = new_offset;
                //     },
                //     Handle::Asymmetric(in_offset, handle_position, out_offset) => {
                //         if is_out_handle {
                //             *out_offset = position - *handle_position;
                //         } else {
                //             *in_offset = position - *handle_position;
                //         }
                //     },
                // };
                todo!()
            },
            ManipulableItem::EdgeMiddlePosition { from_node, to_node, idx } => {
                state.remove_edge(from_node, to_node);
                // let edge = state.edges.get_mut(&(from_node, to_node))
                //     .ok_or(InvalidEdge{from_node, to_node})?
                //     .as_bezier_edge_mut()
                //     .ok_or(EdgeNotBezier{from_node, to_node})?;
                // let handle = &mut edge.mid_handles[idx];
                // match handle {
                //     Handle::Symmetric(_, handle_position) => {
                //         *handle_position = position;
                //     },
                //     Handle::Asymmetric(_, handle_position, _) => {
                //         *handle_position = position;
                //     },
                // };
            },
            ManipulableItem::NodeLabel{ idx } => {
                todo!("implement node label positioning with offsets")
            },
            ManipulableItem::EdgeLabel{ from_node, to_node } => {
                todo!("implement edge label positioning with offsets")
            },
        };
        Ok(())
    }
}

mod tools;
use tools::*;

#[derive(Debug)]
pub struct ApplicationState {
    next_node: usize,
    nodes: HashMap<usize, Node>,
    /// The Vec contains bezier curve points, excluding the first and last, which are the nodes
    edges: HashMap<(usize, usize), Edge>,
    allocated_size: Option<(u32, u32)>,
    this: Weak<RefCell<Self>>,
    drawing_area: Rc<DrawingArea>,
    tool: Rc<dyn Tool>,
}

impl ApplicationState {
    pub fn new(
        drawing_area: &Rc<DrawingArea>,
        move_tool_button: &RadioButton,
        create_nodes_tool_button: &RadioButton,
        create_edges_tool_button: &RadioButton,
        modify_tool_button: &RadioButton,
    ) -> Rc<RefCell<Self>> {
        let state = Self {
            next_node: 0,
            nodes: HashMap::new(),
            edges: HashMap::new(),
            allocated_size: None,
            this: Weak::new(),
            drawing_area: Rc::clone(drawing_area),
            tool: Rc::new(MoveTool::default()),
        };
        let state = Rc::new(RefCell::new(state));
        {
            let state_weak = Rc::downgrade(&state);
            let mut state = state.borrow_mut();
            state.this = state_weak;
        }

        macro_rules! make_state_wrapper {
            ( $func:ident($($args:ident),*) $(=> $retval:expr)? ) => {
                {
                    let state = Rc::clone(&state);
                    move |$($args),*| {
                        state.borrow_mut().$func($($args),*) // semicolon intentionally on next line
                        $(; $retval)?
                    }
                }
            };
            (  ($state:ident $(, $args:ident)*) $body:tt $(=> $retval:expr)? ) => {
                {
                    let $state = Rc::clone(&state);
                    move |$($args),*| $body
                }
            };
        }
        
        drawing_area.connect_draw(make_state_wrapper!{
            draw(area, ctx) => Inhibit(false)
        });
        drawing_area.connect_size_allocate(make_state_wrapper!{
            set_allocated_size(area, allocation)
        });
    
        drawing_area.add_events(EventMask::BUTTON1_MOTION_MASK | EventMask::BUTTON_PRESS_MASK | EventMask::BUTTON_RELEASE_MASK);
        drawing_area.connect_motion_notify_event(make_state_wrapper!{
            on_drag(area, motion) => Inhibit(false)
        });
        drawing_area.connect_button_press_event(make_state_wrapper!{
            on_press(area, press) => Inhibit(false)
        });
        drawing_area.connect_button_release_event(make_state_wrapper!{
            on_release(area, release) => Inhibit(false)
        });
        

        move_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = MoveTool::setup(&mut state);
                }
            }
        });
        create_nodes_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = CreateNodeTool::setup(&mut state);
                }
            }
        });
        create_edges_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = CreateEdgeTool::setup(&mut state);
                }
            }
        });
        modify_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = ModifyTool::setup(&mut state);
                }
            }
        });

        state
    }
    fn set_allocated_size(&mut self, _: &DrawingArea, allocation: &GdkRectangle) {
        self.allocated_size = Some((
            allocation.width.try_into().unwrap(),
            allocation.height.try_into().unwrap(),
        ));
    }

    fn queue_draw(&self) {
        self.drawing_area.queue_draw();
    }

    fn draw_edge(&self, _: &DrawingArea, ctx: &Context, from_position: Position, to_position: Position, edge: &Edge) {
        match &edge.kind {
            EdgeKind::Bezier(BezierEdge {from_offset, mid_handles, to_offset, ..}) => {
                let draw_bezier_with_handles = |p1, p2, p3, p4| {
                    // println!("{:?} {:?} {:?} {:?}", p1, p2, p3, p4);
                    // Draw handles first, so they're under curve
                    ctx.new_path();
                    // Draw handles in thin grey
                    ctx.set_line_width(2.0);
                    ctx.set_source_rgb(0.5, 0.5, 0.5);
                    ctx.line_to_pos(p1);
                    ctx.line_to_pos(p2);
                    
                    ctx.move_to_pos(p3);
                    ctx.line_to_pos(p4);
                    ctx.stroke().unwrap();

                    // Draw handle ends as blue circles
                    ctx.new_path();
                    ctx.set_line_width(2.0);
                    ctx.set_source_rgb(0.0, 0.0, 1.0);
                    ctx.arc_pos(p2, 4.0, 0.0, std::f64::consts::PI);
                    ctx.arc_pos(p2, 4.0, std::f64::consts::PI, 0.0);
                    ctx.close_path();
                    ctx.fill().unwrap();
                    ctx.arc_pos(p3, 4.0, 0.0, std::f64::consts::PI);
                    ctx.arc_pos(p3, 4.0, std::f64::consts::PI, 0.0);
                    ctx.close_path();
                    ctx.fill().unwrap();

                    // Draw Bezier curve in thick black
                    ctx.new_path();
                    ctx.set_line_width(3.0);
                    ctx.set_source_rgb(0.0, 0.0, 0.0);
                    ctx.move_to_pos(p1);
                    ctx.curve_to_pos(p2, p3, p4);
                    ctx.stroke().unwrap();
                };

                let mut p1 = from_position;
                let mut p2 = p1 + from_offset;
                for handle in mid_handles {
                    let p4 = handle.position();
                    let p3 = p4 + handle.in_offset();

                    draw_bezier_with_handles(p1, p2, p3, p4);

                    p1 = p4;
                    p2 = p1 + handle.out_offset();
                }
                let p4 = to_position;
                let p3 = p4 + to_offset;
                draw_bezier_with_handles(p1, p2, p3, p4);
            },
            EdgeKind::Linear => {
                // Draw edge in thick black
                ctx.new_path();
                ctx.set_line_width(3.0);
                ctx.set_source_rgb(0.0, 0.0, 0.0);
                ctx.move_to_pos(from_position);
                ctx.line_to_pos(to_position);
                ctx.stroke().unwrap();
            },
        };
    }

    fn draw_edges(&self, area: &DrawingArea, ctx: &Context) {
        // For each edge:
        // draw line from p1 to p2, with handle (circle) on p2,
        // draw line from p3 to p4, with handle (circle) on p3
        // Note that p4 coincides with p1 of the next curve
        for ((from, to), edge) in self.edges.iter() {
            let from_position = self.nodes[from].position;
            let to_position = self.nodes[to].position;

            self.draw_edge(area, ctx, from_position, to_position, edge);
        }
    }

    fn draw_nodes(&self, _area: &DrawingArea, ctx: &Context) {
        let draw_node = |position| {
            // Draw nodes ends as black circles
            ctx.new_path();
            ctx.set_source_rgb(0.0, 0.0, 0.0);
            ctx.arc_pos(position, 4.0, 0.0, std::f64::consts::PI);
            ctx.arc_pos(position, 4.0, std::f64::consts::PI, 0.0);
            ctx.close_path();
            ctx.fill().unwrap();
        };
        for (_idx, node) in self.nodes.iter() {
            let position = node.position;
            draw_node(position);
        }
    }

    fn draw(&self, area: &DrawingArea, ctx: &Context) {
        // let size = self.allocated_size.unwrap();

        // Clear canvas with white
        ctx.set_source_rgb(1.0, 1.0, 1.0);
        ctx.paint().expect("failed to paint");

        // Draw edges
        self.draw_edges(area, ctx);

        // Draw nodes
        self.draw_nodes(area, ctx);
    }

    /// Returns the item and the squared distance from the item to the position
    fn find_closest_item(&self, press_position: Position) -> Option<(ManipulableItem, f64)> {
        self.find_closest_item_matching(press_position, |_| true)
    }

    /// Returns the item and the squared distance from the item to the position
    /// Only considers items for which predicate(&item) == true
    fn find_closest_item_matching<P: FnMut(&ManipulableItem) -> bool>(&self, press_position: Position, mut predicate: P) -> Option<(ManipulableItem, f64)> {
        // Find handle closest to position
        let mut closest_item: Option<ManipulableItem> = None;
        let mut closest_squared_distance: Option<f64> = None;

        let mut update_closest = |position: Position, item| {
            let offset = position - press_position;
            let squared_distance = offset.norm();

            if closest_squared_distance == None || squared_distance < closest_squared_distance.unwrap() {
                if predicate(&item) {
                    closest_item = Some(item);
                    closest_squared_distance = Some(squared_distance);
                }
            }
        };

        for (&(from_node, to_node), edge) in self.edges.iter() {
            // TODO: edge label positioning
            if let EdgeKind::Bezier(edge) = &edge.kind {
                let from_position = self.nodes[&from_node].position;
                let to_position = self.nodes[&to_node].position;

                let from_handle_position = from_position + edge.from_offset;
                update_closest(from_handle_position, ManipulableItem::EdgeHandle{
                    from_node,
                    to_node,
                    from_handle: true,
                });

                let to_handle_position = to_position + edge.to_offset;
                update_closest(to_handle_position, ManipulableItem::EdgeHandle{
                    from_node,
                    to_node,
                    from_handle: false,
                });

                let mut p1;
                let mut p2;

                for (idx, handle) in edge.mid_handles.iter().enumerate() {
                    let p4 = handle.position();
                    let p3 = p4 + handle.in_offset();

                    update_closest(p3, ManipulableItem::EdgeMiddleHandle{
                        from_node, to_node, idx, out_handle: false
                    });
                    update_closest(p4, ManipulableItem::EdgeMiddlePosition{
                        from_node, to_node, idx
                    });

                    p1 = p4;
                    p2 = p1 + handle.out_offset();

                    update_closest(p2, ManipulableItem::EdgeMiddleHandle{
                        from_node, to_node, idx, out_handle: true
                    });
                }
            }
        }
        for (&idx, node) in self.nodes.iter() {
            // TODO: node label positioning
            let position = node.position;
            update_closest(position, ManipulableItem::Node{idx});
        }

        closest_item.zip(closest_squared_distance)
    }

    fn on_press(&mut self, _: &DrawingArea, press: &EventButton) {
        let tool = Rc::clone(&self.tool);
        tool.on_press(self, press);
    }

    fn on_drag(&mut self, _: &DrawingArea, motion: &EventMotion) {
        let tool = Rc::clone(&self.tool);
        tool.on_drag(self, motion);
    }

    fn on_release(&mut self, _area: &DrawingArea, press: &EventButton) {
        let tool = Rc::clone(&self.tool);
        tool.on_release(self, press);
    }

    pub fn remove_node(&mut self, node_idx: usize) -> Option<Node> { // TODO: return all edges also?
        // Remove all edges with this node at either end
        self.edges.retain(|&(from_idx, to_idx), _| from_idx != node_idx && to_idx != node_idx);
        self.nodes.remove(&node_idx)
    }

    pub fn add_node(&mut self, node: Node) -> usize {
        let idx = self.next_node;
        self.next_node = self.next_node.checked_add(1).unwrap();
        if let Some(_) = self.nodes.insert(idx, node) {
            panic!("index should have been unused, but was not")
        }
        idx
    }

    pub fn remove_edge(&mut self, from_node: usize, to_node: usize) -> Option<Edge> {
        self.edges.remove(&(from_node, to_node))
    }

    // If the edge already existed, does *not* insert, and gives you the edge back
    pub fn add_edge(&mut self, from_node: usize, to_node: usize, edge: Edge) -> Option<Edge> {
        if !self.edges.contains_key(&(from_node, to_node)) {
            self.edges.insert((from_node, to_node), edge);
            None
        } else {
            Some(edge)
        }
    }
}
