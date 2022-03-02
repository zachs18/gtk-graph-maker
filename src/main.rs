use gtk::cairo::{Context};
use gtk::gdk::keys::constants::C;
use gtk::gdk::{EventMask, EventMotion, Rectangle as GdkRectangle, EventButton};
use gtk::{prelude::*, DrawingArea};
use gtk::{cairo, gdk};
use gtk::{ApplicationWindow, Button, Grid};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
mod position;
use position::*;


#[derive(Debug, Clone, Copy, PartialEq)]
enum Handle {
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
struct BezierEdge {
    label: String,
    from_offset: Offset,
    mid_handles: Vec<Handle>,
    to_offset: Offset,
}

#[derive(Debug)]
enum Edge {
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
struct Node {
    label: String,
    position: Position,
}

#[derive(Debug, Clone, Copy)]
enum MovableThing {
    /// Moving a node
    Node { idx: usize },
    /// Moving the position of a midpoint of a Bezier edge
    EdgeMiddlePosition { from_node: usize, to_node: usize, idx: usize },
    /// Moving the handle of a midpoint of a Bezier edge (out_handle means the second handle for asymmetric, or negative first handle for symmetric)
    EdgeMiddleHandle { from_node: usize, to_node: usize, idx: usize, out_handle: bool },
    /// Moving the handle of an endpoint of a Bezier edge
    EdgeHandle { from_node: usize, to_node: usize, from_handle: bool },
}

#[derive(Debug, Default)]
struct ApplicationState {
    nodes: HashMap<usize, Node>,
    /// The Vec contains bezier curve points, excluding the first and last, which are the nodes
    edges: HashMap<(usize, usize), Edge>,
    allocated_size: Option<(u32, u32)>,
    currently_moving_thing: Option<MovableThing>,
}

impl ApplicationState {
    fn set_allocated_size(&mut self, _: &DrawingArea, allocation: &GdkRectangle) {
        self.allocated_size = Some((
            allocation.width.try_into().unwrap(),
            allocation.height.try_into().unwrap(),
        ));
    }

    fn draw_edge(&self, area: &DrawingArea, ctx: &Context, from_position: Position, to_position: Position, edge: &Edge) {
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
                // Draw edge curve in thick black
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
        // For each bezier curve:
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
        for (_idx, node) in self.nodes.iter() {
            let position = node.position;
            // Draw nodes ends as black circles
            ctx.new_path();
            ctx.set_source_rgb(0.0, 0.0, 0.0);
            ctx.arc_pos(position, 4.0, 0.0, std::f64::consts::PI);
            ctx.arc_pos(position, 4.0, std::f64::consts::PI, 0.0);
            ctx.close_path();
            ctx.fill().unwrap();
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

    fn move_thing(&mut self, thing: MovableThing, position: Position) {
        match thing {
            MovableThing::Node { idx } => {
                self.nodes.get_mut(&idx).unwrap().position = position;
            },
            MovableThing::EdgeHandle { from_node, to_node, from_handle } => {
                let edge = self.edges.get_mut(&(from_node, to_node)).unwrap().as_bezier_edge_mut().unwrap();
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
            MovableThing::EdgeMiddleHandle { from_node, to_node, idx, out_handle: is_out_handle } => {
                let edge = self.edges.get_mut(&(from_node, to_node)).unwrap().as_bezier_edge_mut().unwrap();
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
            MovableThing::EdgeMiddlePosition { from_node, to_node, idx } => {
                let edge = self.edges.get_mut(&(from_node, to_node)).unwrap().as_bezier_edge_mut().unwrap();
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
        }
    }

    fn on_drag(&mut self, area: &DrawingArea, motion: &EventMotion) {
        let position = motion.position().into();
        if let Some(currently_moving_thing) = self.currently_moving_thing {
            self.move_thing(currently_moving_thing, position);
            area.queue_draw();
        }
    }

    /// Returns the thing and the squared distance from the thing to the position
    fn find_closest_thing(&self, press_position: Position) -> Option<(MovableThing, f64)> {
        // Find handle closest to position
        let mut closest_thing: Option<MovableThing> = None;
        let mut closest_squared_distance: Option<f64> = None;

        let mut update_closest = |position: Position, thing| {
            let offset = position - press_position;
            let squared_distance = offset.norm();

            if closest_squared_distance == None || squared_distance < closest_squared_distance.unwrap() {
                closest_thing = Some(thing);
                closest_squared_distance = Some(squared_distance);
            }
        };

        for (&(from_node, to_node), edge) in self.edges.iter() {
            if let Edge::Bezier(edge) = edge {
                let from_position = self.nodes[&from_node].position;
                let to_position = self.nodes[&to_node].position;

                let from_handle_position = from_position + edge.from_offset;
                update_closest(from_handle_position, MovableThing::EdgeHandle{
                    from_node,
                    to_node,
                    from_handle: true,
                });

                let to_handle_position = to_position + edge.to_offset;
                update_closest(to_handle_position, MovableThing::EdgeHandle{
                    from_node,
                    to_node,
                    from_handle: false,
                });

                let mut p1;
                let mut p2;

                for (idx, handle) in edge.mid_handles.iter().enumerate() {
                    let p4 = handle.position();
                    let p3 = p4 + handle.in_offset();

                    update_closest(p3, MovableThing::EdgeMiddleHandle{
                        from_node, to_node, idx, out_handle: false
                    });
                    update_closest(p4, MovableThing::EdgeMiddlePosition{
                        from_node, to_node, idx
                    });

                    p1 = p4;
                    p2 = p1 + handle.out_offset();

                    update_closest(p2, MovableThing::EdgeMiddleHandle{
                        from_node, to_node, idx, out_handle: true
                    });
                }
            }
        }
        for (idx, node) in self.nodes.iter().enumerate() {
            let position = node.1.position;
            update_closest(position, MovableThing::Node{idx});
        }

        closest_thing.zip(closest_squared_distance)
    }

    fn on_press(&mut self, _: &DrawingArea, press: &EventButton) {
        if press.button() == 1 {
            // Find handle closest to current press position
            if let Some((closest_thing, squared_distance)) = self.find_closest_thing(press.position().into()) {
                self.currently_moving_thing = Some(closest_thing);
            }
        }
    }

    fn on_release(&mut self, _area: &DrawingArea, press: &EventButton) {
        if press.button() == 1 {
            self.currently_moving_thing = None;
        }
    }
}


fn build_ui(application: &gtk::Application) {
    let window = ApplicationWindow::new(application);
    // set_visual(&window, None);
    let state: Rc<RefCell<ApplicationState>> = Default::default();
    {
        let mut state = state.borrow_mut();
        state.nodes.insert(0, Node {label: "Start".into(), position: (100.0, 100.0).into()});
        state.nodes.insert(1, Node {label: "End".into(), position: (400.0, 400.0).into()});
        state.edges.insert((0, 1), Edge::Bezier(BezierEdge {
            label: "Test".into(),
            from_offset: (0.0, 300.0).into(),
            to_offset: (0.0, -300.0).into(),
            mid_handles: vec![
                Handle::Symmetric((-50.0, -150.0).into(), (250.0, 250.0).into()),
                Handle::Asymmetric((-50.0, -150.0).into(), (200.0, 200.0).into(), (40.0, 40.0).into()),
            ],
        }));
    }

    macro_rules! make_state_wrapper {
        (
            $func:ident($($args:ident),*) $(=> $retval:expr)?
        ) => {
            {
                let state = Rc::clone(&state);
                move |$($args),*| {
                    state.borrow_mut().$func($($args),*) // semicolon intentionally on next line
                    $(; $retval)?
                }
            }
        }
    }

    window.set_title("Graph Maker Test");
    window.set_default_size(500, 500);
    // window.set_app_paintable(true); // crucial for transparency

    let grid = Grid::new();
    grid.set_hexpand(true);
    grid.set_vexpand(true);
    window.add(&grid);

    let drawing_area = gtk::DrawingArea::new();
    drawing_area.set_hexpand(true);
    drawing_area.set_vexpand(true);
    
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
    grid.add(&drawing_area);

    window.show_all();
}

fn main() {
    let application = gtk::Application::new(
        Some("com.github.zachs18.example.gtk-rs1"),
        Default::default(),
    );

    application.connect_activate(build_ui);

    application.run();
}
