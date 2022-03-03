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
    pub label: String,
    pub from_offset: Offset,
    pub mid_handles: Vec<Handle>,
    pub to_offset: Offset,
}

#[derive(Debug)]
pub enum Edge { // TODO: make EdgeKind and pull label into struct Edge { label, kind }
    Bezier(BezierEdge),
    Linear,
}

impl Edge {
    fn as_bezier_edge(&self) -> Option<&BezierEdge> {
        match self { Edge::Bezier(e) => Some(e), _ => None }
    }
    fn as_bezier_edge_mut(&mut self) -> Option<&mut BezierEdge> {
        match self { Edge::Bezier(e) => Some(e), _ => None }
    }
}

#[derive(Debug)]
pub struct Node {
    pub label: String,
    pub position: Position,
}

#[derive(Debug, Clone, Copy)]
enum MovableItem {
    /// Moving a node
    Node { idx: usize },
    /// Moving the position of a midpoint of a Bezier edge
    EdgeMiddlePosition { from_node: usize, to_node: usize, idx: usize },
    /// Moving the handle of a midpoint of a Bezier edge (out_handle means the second handle for asymmetric, or negative first handle for symmetric)
    EdgeMiddleHandle { from_node: usize, to_node: usize, idx: usize, out_handle: bool },
    /// Moving the handle of an endpoint of a Bezier edge
    EdgeHandle { from_node: usize, to_node: usize, from_handle: bool },
}

#[derive(Debug, Clone, Copy)]
enum Tool {
    Move,
    CreateNodes,
    CreateEdges,
    Modify,
}

impl Default for Tool {
    fn default() -> Tool { Tool::Move }
}

#[derive(Debug)]
pub struct ApplicationState {
    next_node: usize,
    nodes: HashMap<usize, Node>,
    /// The Vec contains bezier curve points, excluding the first and last, which are the nodes
    edges: HashMap<(usize, usize), Edge>,
    allocated_size: Option<(u32, u32)>,
    currently_moving_item: Option<MovableItem>,
    this: Weak<RefCell<Self>>,
    drawing_area: Rc<DrawingArea>,
    tool: Tool,
    /// This is used when making edges
    new_edge_start_node: Option<usize>,
    /// This is used when making edges
    fake_node: Option<usize>,
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
            currently_moving_item: None,
            this: Weak::new(),
            drawing_area: Rc::clone(drawing_area),
            tool: Tool::Move,
            new_edge_start_node: None,
            fake_node: None,
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
                    state.tool = Tool::Move;
                }
            }
        });
        create_nodes_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    state.tool = Tool::CreateNodes;
                }
            }
        });
        create_edges_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    state.tool = Tool::CreateEdges;
                }
            }
        });
        modify_tool_button.connect_toggled(make_state_wrapper!{
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    state.tool = Tool::Modify;
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
        match edge {
            Edge::Bezier(BezierEdge {from_offset, mid_handles, to_offset, ..}) => {
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
            Edge::Linear => {
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

    #[must_use]
    fn move_item(&mut self, item: MovableItem, position: Position) -> Result<(), &'static str> {
        match item {
            MovableItem::Node { idx } => {
                self.nodes.get_mut(&idx).ok_or("Invalid node")?.position = position;
            },
            MovableItem::EdgeHandle { from_node, to_node, from_handle } => {
                let edge = 
                    self.edges
                    .get_mut(&(from_node, to_node))
                    .ok_or("Invalid edge")?
                    .as_bezier_edge_mut()
                    .ok_or("Edge was not Bezier")?;
                let from_node = &self.nodes[&from_node];
                let to_node = &self.nodes[&to_node];
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
            MovableItem::EdgeMiddleHandle { from_node, to_node, idx, out_handle: is_out_handle } => {
                let edge = self.edges.get_mut(&(from_node, to_node))
                    .ok_or("Invalid edge")?
                    .as_bezier_edge_mut()
                    .ok_or("Edge was not Bezier")?;
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
            MovableItem::EdgeMiddlePosition { from_node, to_node, idx } => {
                let edge = self.edges.get_mut(&(from_node, to_node))
                    .ok_or("Invalid edge")?
                    .as_bezier_edge_mut()
                    .ok_or("Edge was not Bezier")?;
                let handle = &mut edge.mid_handles[idx];
                match handle {
                    Handle::Symmetric(_, handle_position) => {
                        *handle_position = position;
                    },
                    Handle::Asymmetric(_, handle_position, _) => {
                        *handle_position = position;
                    },
                };
            }
        };
        Ok(())
    }

    fn on_drag(&mut self, area: &DrawingArea, motion: &EventMotion) {
        let position = motion.position().into();
        match self.tool {
            Tool::Move | Tool::CreateNodes | Tool::CreateEdges => {
                if let Some(currently_moving_item) = self.currently_moving_item {
                    if let Err(e) = self.move_item(currently_moving_item, position) {
                        println!("Error: {}", e);
                    }
                    area.queue_draw();
                }
            },
            _ => {
                todo!()
            },
        };
    }

    /// Returns the item and the squared distance from the item to the position
    fn find_closest_item(&self, press_position: Position) -> Option<(MovableItem, f64)> {
        self.find_closest_item_matching(press_position, |_| true)
    }

    /// Returns the item and the squared distance from the item to the position
    /// Only considers items for which predicate(&item) == true
    fn find_closest_item_matching<P: FnMut(&MovableItem) -> bool>(&self, press_position: Position, mut predicate: P) -> Option<(MovableItem, f64)> {
        // Find handle closest to position
        let mut closest_item: Option<MovableItem> = None;
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
            if let Edge::Bezier(edge) = edge {
                let from_position = self.nodes[&from_node].position;
                let to_position = self.nodes[&to_node].position;

                let from_handle_position = from_position + edge.from_offset;
                update_closest(from_handle_position, MovableItem::EdgeHandle{
                    from_node,
                    to_node,
                    from_handle: true,
                });

                let to_handle_position = to_position + edge.to_offset;
                update_closest(to_handle_position, MovableItem::EdgeHandle{
                    from_node,
                    to_node,
                    from_handle: false,
                });

                let mut p1;
                let mut p2;

                for (idx, handle) in edge.mid_handles.iter().enumerate() {
                    let p4 = handle.position();
                    let p3 = p4 + handle.in_offset();

                    update_closest(p3, MovableItem::EdgeMiddleHandle{
                        from_node, to_node, idx, out_handle: false
                    });
                    update_closest(p4, MovableItem::EdgeMiddlePosition{
                        from_node, to_node, idx
                    });

                    p1 = p4;
                    p2 = p1 + handle.out_offset();

                    update_closest(p2, MovableItem::EdgeMiddleHandle{
                        from_node, to_node, idx, out_handle: true
                    });
                }
            }
        }
        for (&idx, node) in self.nodes.iter() {
            let position = node.position;
            update_closest(position, MovableItem::Node{idx});
        }

        closest_item.zip(closest_squared_distance)
    }

    fn on_press(&mut self, _: &DrawingArea, press: &EventButton) {
        let press_position = press.position().into();
        match self.tool {
            Tool::Move => {
                match press.button() {
                    1 => {
                        // Find item closest to current press position
                        if let Some((closest_item, squared_distance)) = self.find_closest_item(press_position) {
                            if squared_distance < 1024.0 {
                                self.currently_moving_item = Some(closest_item);
                            }
                        }
                    },
                    3 => {
                        // Find item closest to current press position
                        let closest_item = match self.find_closest_item(press_position) {
                            Some((closest_item, squared_distance)) if squared_distance < 1024.0 => Some(closest_item),
                            _ => None,
                        };
                        let menu = Menu::new();
                        dbg!(closest_item);
                        if let Some(closest_item) = closest_item {
                            match closest_item {
                                MovableItem::Node { idx } => {
                                    let label_item = MenuItem::new();
                                    label_item.set_label(&format!("Node {} ({:?})", idx, self.nodes[&idx].label));
                                    label_item.show();
                                    label_item.set_sensitive(false);
                                    // dbg!()
                                    menu.attach(&label_item, 0, 1, 0, 1);

                                    let remove_node_item = MenuItem::new();
                                    remove_node_item.set_label("Remove node");
                                    remove_node_item.connect_activate({
                                        let state = Weak::clone(&self.this);
                                        move |remove_node_item| {
                                            dbg!("test1");
                                            if let Some(state) = state.upgrade() {
                                                dbg!("test2");
                                                let mut state = state.borrow_mut();
                                                dbg!("test3");
                                                state.remove_node(idx);
                                                dbg!("test4");
                                                state.queue_draw();
                                            }
                                        }
                                    });
                                    menu.attach(&remove_node_item, 0, 1, 1, 2);
                                },
                                _ => {}
                            };
                            menu.show_all();
                            menu.popup_at_pointer(Some(press));
                        } else {
                            // TODO: Handle right-clicking on empty canvas
                        }
                    },
                    _ => {},
                };
            },
            Tool::CreateNodes => {
                match press.button() {
                    1 => {
                        let idx = self.add_node(Node{
                            label: String::new(),
                            position: press_position,
                        });
                        self.currently_moving_item = Some(MovableItem::Node{idx});
                        self.queue_draw();
                    },
                    _ => {}
                }
            },
            Tool::CreateEdges => {
                // Idea: Click on a node, create a "fake" node,
                //  and create an edge between the clicked node and the fake node,
                //  dragging moves the "fake" node, releasing near another node makes an edge.
                //  releasing NOT near a node deletes the edge.
                //  either way the fake node is deleted.
                // If making an edge from a node to itself, make bezier.
                //  otherwise, linear
                if let Some((MovableItem::Node{idx}, closest_squared_distance)) = self.find_closest_item(press_position) {
                    let start_node = idx;
                    let fake_node = self.add_node(Node{label: "<fake node>".into(), position: press_position});
                    self.fake_node = Some(fake_node);
                    self.new_edge_start_node = Some(start_node);
                    self.currently_moving_item = Some(MovableItem::Node{idx: fake_node});

                    self.add_edge(start_node, fake_node, Edge::Linear);
                    self.queue_draw();
                }
            },
            Tool::Modify => {
                todo!();
            },
        };
    }

    fn on_release(&mut self, _area: &DrawingArea, press: &EventButton) {
        let release_position = press.position().into();
        match self.tool {
            Tool::Move | Tool::CreateNodes => {
                if press.button() == 1 {
                   self.currently_moving_item = None;
                }
            },
            Tool::CreateEdges => {
                let fake_node = match self.fake_node.take() {
                    Some(fake_node) => fake_node,
                    None => return,
                };
                let new_edge_start_node = match self.new_edge_start_node.take() {
                    Some(new_edge_start_node) => new_edge_start_node,
                    None => return,
                };
                // Delete fake node before finding, so we don't find_closest the fake node
                self.remove_node(fake_node);
                let closest_node = self.find_closest_item_matching(
                    release_position,
                    |item| matches!(item, MovableItem::Node{..})
                );
                if let Some((MovableItem::Node{idx: end_node}, squared_distance)) = closest_node {
                    // Add new edge
                    dbg!(end_node);
                    self.add_edge(new_edge_start_node, end_node, Edge::Linear);
                }
                self.queue_draw();
            },
            _ => {
                todo!()
            },
        }
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

    pub fn add_edge(&mut self, from_node: usize, to_node: usize, edge: Edge) -> Option<Edge> {
        self.edges.insert((from_node, to_node), edge)
    }
}
