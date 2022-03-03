use std::{rc::{Rc, Weak}, cell::RefCell};

use gtk::{gdk::{EventButton, EventMotion}, Menu, MenuItem, prelude::{GtkMenuItemExt, GtkMenuExt, WidgetExt}};

use super::{ApplicationState, MovableItem, Node, Edge};

pub trait Tool : std::fmt::Debug {
    fn setup(state: &mut ApplicationState) -> Rc<Self> where Self: Sized;
    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton);
    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion);
    fn on_release(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton);
    fn cleanup(self: Rc<Self>, state: &mut ApplicationState);
}

#[derive(Default, Debug)]
pub struct MoveTool {
    currently_moving_item: RefCell<Option<MovableItem>>,
}

impl Tool for MoveTool {
    fn setup(_state: &mut ApplicationState) -> Rc<Self> where Self: Sized {
        Self::default().into()
    }
    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let mut currently_moving_item = self.currently_moving_item.borrow_mut();
        let press_position = press.position().into();
        match press.button() {
            1 => {
                // Find item closest to current press position
                if let Some((closest_item, squared_distance)) = state.find_closest_item(press_position) {
                    if squared_distance < 1024.0 {
                        *currently_moving_item = Some(closest_item);
                    }
                }
            },
            3 => {
                // Find item closest to current press position
                let closest_item = match state.find_closest_item(press_position) {
                    Some((closest_item, squared_distance)) if squared_distance < 1024.0 => Some(closest_item),
                    _ => None,
                };
                let menu = Menu::new();
                dbg!(closest_item);
                if let Some(closest_item) = closest_item {
                    match closest_item {
                        MovableItem::Node { idx } => {
                            let label_item = MenuItem::new();
                            label_item.set_label(&format!("Node {} ({:?})", idx, state.nodes[&idx].label));
                            label_item.show();
                            label_item.set_sensitive(false);
                            // dbg!()
                            menu.attach(&label_item, 0, 1, 0, 1);

                            let remove_node_item = MenuItem::new();
                            remove_node_item.set_label("Remove node");
                            remove_node_item.connect_activate({
                                let state = Weak::clone(&state.this);
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
    }
    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion) {
        let position = motion.position().into();
        let currently_moving_item = self.currently_moving_item.borrow_mut();
        if let Some(currently_moving_item) = *currently_moving_item {
            if let Err(e) = state.move_item(currently_moving_item, position) {
                println!("Error: {}", e);
            }
            state.queue_draw();
        }
    }
    fn on_release(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        if press.button() == 1 {
            let mut currently_moving_item = self.currently_moving_item.borrow_mut();
            *currently_moving_item = None;
        }
    }
    fn cleanup(self: Rc<Self>, state: &mut ApplicationState) {
        let mut currently_moving_item = self.currently_moving_item.borrow_mut();
        *currently_moving_item = None;
    }
}


#[derive(Default, Debug)]
pub struct CreateNodeTool {
    currrent_node: RefCell<Option<MovableItem>>,
}

impl Tool for CreateNodeTool {
    fn setup(_state: &mut ApplicationState) -> Rc<Self> where Self: Sized {
        Self::default().into()
    }
    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let mut currrent_node = self.currrent_node.borrow_mut();
        let press_position = press.position().into();
        match press.button() {
            1 => {
                let idx = state.add_node(Node{
                    label: String::new(),
                    position: press_position,
                });
                *currrent_node = Some(MovableItem::Node{idx});
                state.queue_draw();
            },
            _ => {}
        }
    }
    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion) {
        let position = motion.position().into();
        let currrent_node = self.currrent_node.borrow_mut();
        if let Some(currrent_node) = *currrent_node {
            if let Err(e) = state.move_item(currrent_node, position) {
                println!("Error: {}", e);
            }
            state.queue_draw();
        }
    }
    fn on_release(self: Rc<Self>, _state: &mut ApplicationState, press: &EventButton) {
        let mut currrent_node = self.currrent_node.borrow_mut();
        if press.button() == 1 {
            *currrent_node = None;
        }
    }
    fn cleanup(self: Rc<Self>, _state: &mut ApplicationState) {
        let mut currrent_node = self.currrent_node.borrow_mut();
        *currrent_node = None;
    }
}


#[derive(Default, Debug)]
pub struct CreateEdgeToolInner {
    from_node: usize,
    fake_to_node: usize,
}

#[derive(Default, Debug)]
pub struct CreateEdgeTool {
    inner: RefCell<Option<CreateEdgeToolInner>>,
}

impl Tool for CreateEdgeTool {
    fn setup(_state: &mut ApplicationState) -> Rc<Self> where Self: Sized {
        Rc::new(Self::default())
    }

    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let press_position = press.position().into();
        let mut inner = self.inner.borrow_mut();
        // Idea: Click on a node, create a "fake" node,
        //  and create an edge between the clicked node and the fake node,
        //  dragging moves the "fake" node, releasing near another node makes an edge.
        //  releasing NOT near a node deletes the edge.
        //  either way the fake node is deleted.
        // If making an edge from a node to itself, make bezier.
        //  otherwise, linear
        if let Some((MovableItem::Node{idx}, _)) = state.find_closest_item(press_position) {
            let from_node = idx;
            let fake_to_node = state.add_node(Node{label: "<fake node>".into(), position: press_position});
            *inner = Some(CreateEdgeToolInner {from_node, fake_to_node});

            state.add_edge(from_node, fake_to_node, Edge::Linear);
            state.queue_draw();
        }
    }

    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion) {
        let position = motion.position().into();
        let inner = self.inner.borrow_mut();
        if let Some(inner) = &*inner {
            if let Err(e) = state.move_item(MovableItem::Node{idx: inner.fake_to_node}, position) {
                println!("Error: {}", e);
            }
            state.queue_draw();
        }
    }

    fn on_release(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let release_position = press.position().into();
        let mut inner = self.inner.borrow_mut();
        let CreateEdgeToolInner { from_node, fake_to_node } = match inner.take() {
            Some(inner) => inner,
            None => return,
        };
        // Delete fake node before finding, so we don't find_closest the fake node
        state.remove_node(fake_to_node);
        let closest_node = state.find_closest_item_matching(
            release_position,
            |item| matches!(item, MovableItem::Node{..})
        );
        if let Some((MovableItem::Node{idx: to_node}, _)) = closest_node {
            // Add new edge
            dbg!(to_node);
            state.add_edge(from_node, to_node, Edge::Linear);
        }
        state.queue_draw();
    }

    fn cleanup(self: Rc<Self>, state: &mut ApplicationState) {
        let mut inner = self.inner.borrow_mut();
        let CreateEdgeToolInner { fake_to_node, .. } = match inner.take() {
            Some(inner) => inner,
            None => return,
        };
        state.remove_node(fake_to_node);
    }
}