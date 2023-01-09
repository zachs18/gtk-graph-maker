use crate::position::*;
use gtk::prelude::{IsA, ToggleButtonExt};
use gtk::traits::{ContainerExt, DialogExt, EntryExt};
use gtk::{
    cairo::Context,
    gdk::{EventButton, EventMask, EventMotion, Rectangle as GdkRectangle},
    prelude::{WidgetExt, WidgetExtManual},
    DrawingArea, Inhibit,
};
use gtk::{ApplicationWindow, RadioButton, Widget};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

mod edge;
pub use edge::*;

#[derive(Debug)]
pub struct Node {
    pub label: Option<Cow<'static, str>>,
    pub position: Position,
}

#[derive(Debug, Clone, Copy)]
enum ManipulableItem {
    /// Manipulating a node
    Node { idx: usize },
    /// Manipulating a control_point of an edge
    EdgeMiddleControlPoint {
        from_node: usize,
        to_node: usize,
        control_point_idx: usize,
    },
    /// Manipulating the handle of a control_point of a Bezier edge (out_handle means the second handle for asymmetric, or negative first handle for symmetric)
    EdgeHandle {
        from_node: usize,
        to_node: usize,
        segment_idx: usize,
        out_handle: bool,
    },
    /// Right-clicking (not moving) the body of a segment of an edge.
    EdgeBody {
        from_node: usize,
        to_node: usize,
        segment_idx: usize,
    },
    /// Manipulating a node label
    #[allow(unused)]
    NodeLabel { idx: usize },
    /// Manipulating an edge label
    #[allow(unused)]
    EdgeLabel { from_node: usize, to_node: usize },
}

pub struct Action {
    description: Cow<'static, str>,
    action: Box<dyn Fn(&RefCell<ApplicationState>) -> Result<(), ManipulateError>>,
}

impl Action {
    pub fn modal_dialog_action<W: IsA<Widget> + 'static>(
        description: Cow<'static, str>,
        dialog_name: impl AsRef<str> + 'static,
        dialog_widget: &W,
        on_ok: impl Fn(&W, &RefCell<ApplicationState>) -> Result<(), ManipulateError> + 'static,
        on_cancel: impl Fn(&W, &RefCell<ApplicationState>) -> Result<(), ManipulateError> + 'static,
    ) -> Self {
        let dialog_widget: W = dialog_widget.clone();
        Action {
            description,
            action: Box::new(move |state| {
                let label_dialog = gtk::Dialog::with_buttons(
                    Some(dialog_name.as_ref()),
                    Some(&state.borrow().window),
                    gtk::DialogFlags::MODAL,
                    &[
                        ("Ok", gtk::ResponseType::Ok),
                        ("Cancel", gtk::ResponseType::Cancel),
                    ],
                );
                label_dialog.content_area().add(&dialog_widget);
                label_dialog.show_all();
                // Don't have a borrow of `state` while the dialog is running,
                // since it runs a main loop which may access state
                let response = label_dialog.run();
                label_dialog.hide();
                match response {
                    gtk::ResponseType::Accept
                    | gtk::ResponseType::Ok
                    | gtk::ResponseType::Yes
                    | gtk::ResponseType::Apply => {
                        on_ok(&dialog_widget, state)?;
                    }
                    _ => {
                        on_cancel(&dialog_widget, state)?;
                    }
                }
                state.borrow_mut().queue_draw();
                Ok(())
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ManipulateError {
    InvalidNode {
        idx: usize,
    },
    InvalidEdge {
        from_node: usize,
        to_node: usize,
    },
    ManipulateEdgeError {
        from_node: usize,
        to_node: usize,
        error: ManipulateEdgeError,
    },
    ItemHasNoLabel,
    ItemIsNotEdgeHandle,
    ItemIsNotMovable,
}

impl ManipulateError {
    fn add_nodes_to_edge_error(
        from_node: usize,
        to_node: usize,
    ) -> impl Fn(ManipulateEdgeError) -> ManipulateError + Copy {
        move |error| ManipulateError::ManipulateEdgeError {
            from_node,
            to_node,
            error,
        }
    }
}

impl ManipulableItem {
    pub fn actions(&self, state: &mut ApplicationState) -> (String, Vec<Action>) {
        let this = *self;
        let description = format!("{self:?}");
        let mut actions = vec![];
        actions.push(Action {
            description: "Remove".into(),
            action: Box::new(move |state| this.remove_item(&mut state.borrow_mut())),
        });
        // if self.is_label() || self.has_label() {
        //     actions.push(Action {
        //         description: "Remove label".into(),
        //         action: Box::new(move |state| this.move_item(state)),
        //     });
        // }
        if self.is_label() || self.can_have_label() {
            match self.get_label(state).expect("self can have label") {
                None => {
                    let add_label_entry = gtk::Entry::new();
                    actions.push(Action::modal_dialog_action(
                        "Add label".into(),
                        format!("Adding label to {this:?}"),
                        &add_label_entry,
                        move |add_label_entry, state| {
                            this.replace_label(
                                &mut state.borrow_mut(),
                                Some(add_label_entry.buffer().text().into()),
                            )?;
                            Ok(())
                        },
                        |_, _| Ok(()),
                    ));
                }
                Some(label) => {
                    let change_label_entry =
                        gtk::Entry::with_buffer(&gtk::EntryBuffer::new(Some(label)));
                    actions.push(Action::modal_dialog_action(
                        "Change label".into(),
                        format!("Changing label of {this:?}"),
                        &change_label_entry,
                        move |label_entry, state| {
                            this.replace_label(
                                &mut state.borrow_mut(),
                                Some(label_entry.buffer().text().into()),
                            )?;
                            Ok(())
                        },
                        |_, _| Ok(()),
                    ));
                    actions.push(Action {
                        description: "Remove label".into(),
                        action: Box::new(move |state| {
                            this.replace_label(&mut state.borrow_mut(), None)?;
                            Ok(())
                        }),
                    })
                }
            }
        }
        (description, actions)
    }

    pub fn is_movable(&self) -> bool {
        use ManipulableItem::*;
        match self {
            Node { .. } => true,
            EdgeMiddleControlPoint { .. } => true,
            EdgeHandle { .. } => true,
            NodeLabel { .. } => true,
            EdgeLabel { .. } => true,
            EdgeBody { .. } => false,
        }
    }

    pub fn move_item(
        &self,
        state: &mut ApplicationState,
        position: Position,
    ) -> Result<(), ManipulateError> {
        use ManipulateError::*;
        match *self {
            ManipulableItem::EdgeBody { .. } => Err(ManipulateError::ItemIsNotMovable),
            ManipulableItem::Node { idx } => {
                state
                    .nodes
                    .get_mut(&idx)
                    .ok_or(InvalidNode { idx })?
                    .position = position;
                Ok(())
            }
            ManipulableItem::EdgeHandle {
                from_node,
                to_node,
                segment_idx,
                out_handle: is_out_handle,
            } => {
                let edge = state
                    .edges
                    .get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge { from_node, to_node })?;
                let initial_position = &state.nodes[&from_node].position;
                let terminal_position = &state.nodes[&to_node].position;
                let mut handle = edge
                    .handle(
                        initial_position,
                        terminal_position,
                        segment_idx + !is_out_handle as usize,
                    )
                    .ok_or(ManipulateEdgeError {
                        from_node,
                        to_node,
                        error: edge::ManipulateEdgeError::InvalidControlPoint {
                            control_point_idx: segment_idx,
                        },
                    })?;
                if is_out_handle {
                    *handle.out_offset().map_err(move |error| {
                        ManipulateError::ManipulateEdgeError {
                            from_node,
                            to_node,
                            error,
                        }
                    })? = position - *handle.position();
                } else {
                    *handle.in_offset().map_err(move |error| {
                        ManipulateError::ManipulateEdgeError {
                            from_node,
                            to_node,
                            error,
                        }
                    })? = position - *handle.position();
                }
                Ok(())
            }
            ManipulableItem::EdgeMiddleControlPoint {
                from_node,
                to_node,
                control_point_idx: idx,
            } => {
                let edge = state
                    .edges
                    .get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge { from_node, to_node })?;
                edge.control_points[idx - 1] = position;
                Ok(())
            }
            ManipulableItem::NodeLabel { .. } => {
                todo!("implement node label positioning with offsets")
            }
            ManipulableItem::EdgeLabel { .. } => {
                todo!("implement edge label positioning with offsets")
            }
        }
    }

    pub fn is_label(&self) -> bool {
        use ManipulableItem::*;
        match self {
            NodeLabel { .. } => true,
            EdgeLabel { .. } => true,
            _ => false,
        }
    }

    pub fn replace_label(
        &self,
        state: &mut ApplicationState,
        new_label: Option<Cow<'static, str>>,
    ) -> Result<Option<Cow<'static, str>>, ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        match *self {
            Node { idx } | NodeLabel { idx } => {
                let node = state.nodes.get_mut(&idx).ok_or(InvalidNode { idx })?;
                Ok(std::mem::replace(&mut node.label, new_label))
            }
            EdgeMiddleControlPoint {
                from_node, to_node, ..
            }
            | EdgeHandle {
                from_node, to_node, ..
            }
            | EdgeBody {
                from_node, to_node, ..
            }
            | EdgeLabel { from_node, to_node } => {
                let edge = state
                    .edges
                    .get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge { from_node, to_node })?;
                Ok(std::mem::replace(&mut edge.label, new_label))
            }
        }
    }

    pub fn get_label<'a>(
        &self,
        state: &'a ApplicationState,
    ) -> Result<Option<&'a str>, ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        match *self {
            Node { idx } | NodeLabel { idx } => {
                let node = state.nodes.get(&idx).ok_or(InvalidNode { idx })?;
                Ok(node.label.as_deref())
            }
            EdgeMiddleControlPoint {
                from_node, to_node, ..
            }
            | EdgeHandle {
                from_node, to_node, ..
            }
            | EdgeBody {
                from_node, to_node, ..
            }
            | EdgeLabel { from_node, to_node } => {
                let edge = state
                    .edges
                    .get(&(from_node, to_node))
                    .ok_or(InvalidEdge { from_node, to_node })?;
                Ok(edge.label.as_deref())
            }
        }
    }

    pub fn make_symmetric(&self, state: &mut ApplicationState) -> Result<(), ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        if let EdgeHandle {
            from_node, to_node, ..
        }
        | EdgeMiddleControlPoint {
            from_node, to_node, ..
        } = *self
        {
            let edge = state
                .edges
                .get_mut(&(from_node, to_node))
                .ok_or(InvalidEdge { from_node, to_node })?;
            let initial_position = &state.nodes[&from_node].position;
            let terminal_position = &state.nodes[&to_node].position;
            let control_point_idx = match *self {
                EdgeMiddleControlPoint {
                    control_point_idx, ..
                } => control_point_idx,
                EdgeHandle {
                    segment_idx,
                    out_handle,
                    ..
                } => segment_idx + !out_handle as usize,
                _ => unreachable!(),
            };
            edge.make_symmetric(initial_position, terminal_position, control_point_idx)
                .map_err(ManipulateError::add_nodes_to_edge_error(from_node, to_node))?;
            Ok(())
        } else {
            Err(ItemIsNotEdgeHandle)
        }
    }
    pub fn make_asymmetric(&self, state: &mut ApplicationState) -> Result<(), ManipulateError> {
        use ManipulableItem::*;
        use ManipulateError::*;
        if let EdgeHandle {
            from_node, to_node, ..
        }
        | EdgeMiddleControlPoint {
            from_node, to_node, ..
        } = *self
        {
            let edge = state
                .edges
                .get_mut(&(from_node, to_node))
                .ok_or(InvalidEdge { from_node, to_node })?;
            let from_position = &state.nodes[&from_node].position;
            let to_position = &state.nodes[&to_node].position;
            let control_point_idx = match *self {
                EdgeMiddleControlPoint {
                    control_point_idx, ..
                } => control_point_idx,
                EdgeHandle {
                    segment_idx,
                    out_handle,
                    ..
                } => segment_idx + !out_handle as usize,
                _ => unreachable!(),
            };
            edge.make_asymmetric(from_position, to_position, control_point_idx)
                .map_err(move |error| ManipulateError::ManipulateEdgeError {
                    from_node,
                    to_node,
                    error,
                })
        } else {
            Err(ItemIsNotEdgeHandle)
        }
    }

    pub fn remove_item(&self, state: &mut ApplicationState) -> Result<(), ManipulateError> {
        use ManipulateError::*;
        match *self {
            ManipulableItem::Node { idx } => {
                state.remove_node(idx);
                Ok(())
            }
            ManipulableItem::EdgeHandle {
                from_node,
                to_node,
                segment_idx,
                ..
            } => {
                dbg!(self);
                let edge = state
                    .edges
                    .get_mut(&(from_node, to_node))
                    .ok_or(InvalidEdge { from_node, to_node })?;
                edge.make_linear(segment_idx).map_err(move |error| {
                    ManipulateError::ManipulateEdgeError {
                        from_node: from_node,
                        to_node: to_node,
                        error,
                    }
                })
            }
            ManipulableItem::EdgeMiddleControlPoint {
                from_node,
                to_node,
                control_point_idx: _,
            } => {
                eprintln!("TODO: just remove the control point (combine two segments), don't remove the whole edge");
                state.remove_edge(from_node, to_node);
                Ok(())
            }
            ManipulableItem::EdgeBody {
                from_node,
                to_node,
                segment_idx,
            } => {
                eprintln!("TODO: remove the whole edge, or just this segment? Probably remove whole edge if last segment, else just remove the segment");
                state.remove_edge(from_node, to_node);
                Ok(())
            }
            ManipulableItem::NodeLabel { .. } => {
                todo!("implement node label positioning with offsets")
            }
            ManipulableItem::EdgeLabel { .. } => {
                todo!("implement edge label positioning with offsets")
            }
        }
    }

    pub(crate) fn can_have_label(&self) -> bool {
        use ManipulableItem::*;
        match self {
            Node { .. } => true,
            EdgeMiddleControlPoint { .. } => true,
            EdgeHandle { .. } => true,
            EdgeBody { .. } => true,
            NodeLabel { .. } => false,
            EdgeLabel { .. } => false,
        }
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
    drawing_area: DrawingArea,
    window: ApplicationWindow,
    tool: Rc<dyn Tool>,
}

impl ApplicationState {
    pub fn new(
        window: ApplicationWindow,
        drawing_area: DrawingArea,
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
            drawing_area: drawing_area.clone(),
            window,
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

        drawing_area.connect_draw(make_state_wrapper! {
            draw(area, ctx) => Inhibit(false)
        });
        drawing_area.connect_size_allocate(make_state_wrapper! {
            set_allocated_size(area, allocation)
        });

        drawing_area.add_events(
            EventMask::BUTTON1_MOTION_MASK
                | EventMask::BUTTON_PRESS_MASK
                | EventMask::BUTTON_RELEASE_MASK,
        );
        drawing_area.connect_motion_notify_event(make_state_wrapper! {
            on_drag(area, motion) => Inhibit(false)
        });
        drawing_area.connect_button_press_event(make_state_wrapper! {
            on_press(area, press) => Inhibit(false)
        });
        drawing_area.connect_button_release_event(make_state_wrapper! {
            on_release(area, release) => Inhibit(false)
        });

        move_tool_button.connect_toggled(make_state_wrapper! {
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = MoveTool::setup(&mut state);
                }
            }
        });
        create_nodes_tool_button.connect_toggled(make_state_wrapper! {
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = CreateNodeTool::setup(&mut state);
                }
            }
        });
        create_edges_tool_button.connect_toggled(make_state_wrapper! {
            (state, button) {
                if button.is_active() {
                    let mut state = state.borrow_mut();
                    let tool = Rc::clone(&state.tool);
                    tool.cleanup(&mut state);
                    state.tool = CreateEdgeTool::setup(&mut state);
                }
            }
        });
        modify_tool_button.connect_toggled(make_state_wrapper! {
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
            allocation.width().try_into().unwrap(),
            allocation.height().try_into().unwrap(),
        ));
    }

    fn queue_draw(&self) {
        self.drawing_area.queue_draw();
    }

    fn draw_edge(
        &self,
        _: &DrawingArea,
        ctx: &Context,
        from_position: Position,
        to_position: Position,
        edge: &Edge,
    ) {
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
        for (segment, &p4) in edge.segments.iter().zip(
            edge.control_points
                .iter()
                .chain(std::iter::once(&to_position)),
        ) {
            match segment {
                EdgeSegment::Linear => {
                    // Draw edge in thick black
                    ctx.new_path();
                    ctx.set_line_width(3.0);
                    ctx.set_source_rgb(0.0, 0.0, 0.0);
                    ctx.move_to_pos(p1);
                    ctx.line_to_pos(p4);
                    ctx.stroke().unwrap();
                }
                EdgeSegment::Bezier(segment) => {
                    let p2 = p1 + segment.from_offset;
                    let p3 = p4 + segment.to_offset;
                    draw_bezier_with_handles(p1, p2, p3, p4);
                }
            }
            p1 = p4;
        }
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

    fn draw_labels(&self, _area: &DrawingArea, ctx: &Context) {
        // let draw_label = |position, label| ctx.show_text(text);

        for (_idx, node) in self.nodes.iter() {
            if let Some(label) = node.label.as_deref() {
                let position = node.position;
                ctx.move_to_pos(position);
                // TODO: use ctx.show_text_glyphs, which also can allows centering text.
                ctx.show_text(label).expect("handle error?");
            }
        }

        // TODO: edge labels
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

        // Draw labels
        self.draw_labels(area, ctx);
    }

    /// Returns the item and the squared distance from the item to the position
    fn find_closest_item(&self, press_position: Position) -> Option<(ManipulableItem, f64)> {
        self.find_closest_item_matching(press_position, |_| true)
    }

    /// Returns the item and the squared distance from the item to the position
    /// Only considers items for which predicate(&item) == true
    fn find_closest_item_matching<P: FnMut(&ManipulableItem) -> bool>(
        &self,
        press_position: Position,
        mut predicate: P,
    ) -> Option<(ManipulableItem, f64)> {
        // Find handle closest to position
        let mut closest_item: Option<ManipulableItem> = None;
        let mut closest_squared_distance: Option<f64> = None;

        let mut update_closest = |position: Position, item| {
            if predicate(&item) {
                let offset = position - press_position;
                let squared_distance = offset.norm();

                if closest_squared_distance == None
                    || squared_distance < closest_squared_distance.unwrap()
                {
                    closest_item = Some(item);
                    closest_squared_distance = Some(squared_distance);
                }
            }
        };

        for (&(from_node, to_node), edge) in self.edges.iter() {
            let from_position = self.nodes[&from_node].position;
            let to_position = self.nodes[&to_node].position;

            let mut p1 = from_position;
            for (idx, (segment, &p4)) in edge
                .segments
                .iter()
                .zip(
                    edge.control_points
                        .iter()
                        .chain(std::iter::once(&to_position)),
                )
                .enumerate()
            {
                if idx < edge.control_points.len() {
                    update_closest(
                        p4,
                        ManipulableItem::EdgeMiddleControlPoint {
                            from_node,
                            to_node,
                            control_point_idx: idx + 1,
                        },
                    );
                }
                match segment {
                    EdgeSegment::Linear => {
                        // Linear segments have no internal points.
                    }
                    EdgeSegment::Bezier(segment) => {
                        let p2 = p1 + segment.from_offset;
                        let p3 = p4 + segment.to_offset;
                        update_closest(
                            p2,
                            ManipulableItem::EdgeHandle {
                                from_node,
                                to_node,
                                segment_idx: idx,
                                out_handle: true,
                            },
                        );
                        update_closest(
                            p3,
                            ManipulableItem::EdgeHandle {
                                from_node,
                                to_node,
                                segment_idx: idx,
                                out_handle: false,
                            },
                        );
                    }
                }
                p1 = p4;
            }
            // TODO: edge label positioning
            #[cfg(any())]
            if let EdgeKind::Bezier(edge) = &edge.kind {
                let from_handle_position = from_position + edge.from_offset;
                update_closest(
                    from_handle_position,
                    ManipulableItem::EdgeHandle {
                        from_node,
                        to_node,
                        from_handle: true,
                    },
                );

                let to_handle_position = to_position + edge.to_offset;
                update_closest(
                    to_handle_position,
                    ManipulableItem::EdgeHandle {
                        from_node,
                        to_node,
                        from_handle: false,
                    },
                );

                let mut p1;
                let mut p2;

                for (idx, handle) in edge.mid_handles.iter().enumerate() {
                    let p4 = handle.position();
                    let p3 = p4 + handle.in_offset();

                    update_closest(
                        p3,
                        ManipulableItem::EdgeHandle {
                            from_node,
                            to_node,
                            idx,
                            out_handle: false,
                        },
                    );
                    update_closest(
                        p4,
                        ManipulableItem::EdgeMiddleControlPoint {
                            from_node,
                            to_node,
                            idx,
                        },
                    );

                    p1 = p4;
                    p2 = p1 + handle.out_offset();

                    update_closest(
                        p2,
                        ManipulableItem::EdgeHandle {
                            from_node,
                            to_node,
                            idx,
                            out_handle: true,
                        },
                    );
                }
            }
        }
        for (&idx, node) in self.nodes.iter() {
            // TODO: node label positioning
            let position = node.position;
            update_closest(position, ManipulableItem::Node { idx });
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

    pub fn remove_node(&mut self, node_idx: usize) -> Option<Node> {
        // TODO: return all edges also?
        // Remove all edges with this node at either end
        self.edges
            .retain(|&(from_idx, to_idx), _| from_idx != node_idx && to_idx != node_idx);
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
