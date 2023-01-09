use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use gtk::{
    gdk::{EventButton, EventMotion},
    prelude::{GtkMenuExt, GtkMenuItemExt, WidgetExt},
    Menu, MenuItem,
};

use super::{ApplicationState, Edge, ManipulableItem, Node};

pub trait Tool: std::fmt::Debug {
    fn setup(state: &mut ApplicationState) -> Rc<Self>
    where
        Self: Sized;
    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton);
    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion);
    fn on_release(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton);
    fn cleanup(self: Rc<Self>, state: &mut ApplicationState);
}

#[derive(Default, Debug)]
pub struct MoveTool {
    currently_moving_item: RefCell<Option<ManipulableItem>>,
}

impl Tool for MoveTool {
    fn setup(_state: &mut ApplicationState) -> Rc<Self>
    where
        Self: Sized,
    {
        Self::default().into()
    }
    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let mut currently_moving_item = self.currently_moving_item.borrow_mut();
        let press_position = press.position().into();
        match press.button() {
            1 => {
                // Find item closest to current press position
                if let Some((closest_item, squared_distance)) =
                    state.find_closest_item(press_position)
                {
                    if squared_distance < 1024.0 {
                        *currently_moving_item = Some(closest_item);
                    }
                }
            }
            3 => {
                // Behave as ModifyTool when right-clicking
                let tool = Rc::new(ModifyTool {});
                tool.on_press(state, press);
            }
            _ => {}
        };
    }
    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion) {
        let position = motion.position().into();
        let currently_moving_item = self.currently_moving_item.borrow_mut();
        if let Some(currently_moving_item) = *currently_moving_item {
            if let Err(e) = currently_moving_item.move_item(state, position) {
                println!("Error: {:?}", e);
            }
            state.queue_draw();
        }
    }
    fn on_release(self: Rc<Self>, _state: &mut ApplicationState, press: &EventButton) {
        if press.button() == 1 {
            self.currently_moving_item.replace(None);
        }
    }
    fn cleanup(self: Rc<Self>, _state: &mut ApplicationState) {
        self.currently_moving_item.replace(None);
    }
}

#[derive(Default, Debug)]
pub struct CreateNodeTool {
    currrent_node: RefCell<Option<ManipulableItem>>,
}

impl Tool for CreateNodeTool {
    fn setup(_state: &mut ApplicationState) -> Rc<Self>
    where
        Self: Sized,
    {
        Self::default().into()
    }
    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let mut currrent_node = self.currrent_node.borrow_mut();
        let press_position = press.position().into();
        match press.button() {
            1 => {
                let idx = state.add_node(Node {
                    label: None,
                    position: press_position,
                });
                *currrent_node = Some(ManipulableItem::Node { idx });
                state.queue_draw();
            }
            _ => {}
        }
    }
    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion) {
        let position = motion.position().into();
        let currrent_node = self.currrent_node.borrow_mut();
        if let Some(currrent_node) = *currrent_node {
            if let Err(e) = currrent_node.move_item(state, position) {
                println!("Error: {:?}", e);
            }
            state.queue_draw();
        }
    }
    fn on_release(self: Rc<Self>, _state: &mut ApplicationState, press: &EventButton) {
        if press.button() == 1 {
            self.currrent_node.replace(None);
        }
    }
    fn cleanup(self: Rc<Self>, _state: &mut ApplicationState) {
        self.currrent_node.replace(None);
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
    fn setup(_state: &mut ApplicationState) -> Rc<Self>
    where
        Self: Sized,
    {
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
        if let Some((ManipulableItem::Node { idx }, _)) = state.find_closest_item(press_position) {
            let from_node = idx;
            let fake_to_node = state.add_node(Node {
                label: None,
                position: press_position,
            });
            *inner = Some(CreateEdgeToolInner {
                from_node,
                fake_to_node,
            });

            state.add_edge(from_node, fake_to_node, Edge::default());
            state.queue_draw();
        }
    }

    fn on_drag(self: Rc<Self>, state: &mut ApplicationState, motion: &EventMotion) {
        let position = motion.position().into();
        let inner = self.inner.borrow_mut();
        if let Some(inner) = &*inner {
            let item = ManipulableItem::Node {
                idx: inner.fake_to_node,
            };
            if let Err(e) = item.move_item(state, position) {
                println!("Error: {:?}", e);
            }
            state.queue_draw();
        }
    }

    fn on_release(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let release_position = press.position().into();
        let mut inner = self.inner.borrow_mut();
        let CreateEdgeToolInner {
            from_node,
            fake_to_node,
        } = match inner.take() {
            Some(inner) => inner,
            None => return,
        };
        // Delete fake node before finding, so we don't find_closest the fake node
        state.remove_node(fake_to_node);
        let closest_node = state.find_closest_item_matching(release_position, |item| {
            matches!(item, ManipulableItem::Node { .. })
        });
        if let Some((ManipulableItem::Node { idx: to_node }, _)) = closest_node {
            // Add new edge
            dbg!(to_node);
            state.add_edge(from_node, to_node, Edge::default());
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

#[derive(Default, Debug)]
pub struct ModifyTool {}

impl Tool for ModifyTool {
    fn setup(_state: &mut ApplicationState) -> Rc<Self>
    where
        Self: Sized,
    {
        Rc::new(Self::default())
    }

    fn on_press(self: Rc<Self>, state: &mut ApplicationState, press: &EventButton) {
        let press_position = press.position().into();
        // Find item closest to current press position
        let closest_item = match state.find_closest_item(press_position) {
            Some((closest_item, squared_distance)) if squared_distance < 1024.0 => {
                Some(closest_item)
            }
            _ => None,
        };
        let menu = Menu::new();
        let mut attach = {
            let menu = &menu;
            let mut row = 0;
            move |child: &MenuItem| {
                menu.attach(child, 0, 1, row, row + 1);
                row += 1;
            }
        };
        dbg!(closest_item);
        if let Some(closest_item) = closest_item {
            let (description, actions) = closest_item.actions(state);

            let label_item = MenuItem::new();
            label_item.set_label(&description); // TODO: description, e.g. including label?
            label_item.show();
            label_item.set_sensitive(false);
            attach(&label_item);

            for action in actions {
                let action_item = MenuItem::new();
                action_item.set_label(&action.description);
                action_item.connect_activate({
                    let state = Weak::clone(&state.this);
                    move |_action_item| {
                        if let Some(state) = state.upgrade() {
                            (action.action)(&state).unwrap();
                            state.borrow_mut().queue_draw();
                        }
                    }
                });
                attach(&action_item);
            }

            // let remove_item = MenuItem::new();
            // remove_item.set_label("Remove");
            // remove_item.connect_activate({
            //     let state = Weak::clone(&state.this);
            //     move |_remove_item| {
            //         if let Some(state) = state.upgrade() {
            //             let mut state = state.borrow_mut();
            //             closest_item.remove_item(&mut state).unwrap();
            //             state.queue_draw();
            //         }
            //     }
            // });
            // attach(&remove_item);

            // if closest_item.is_label() || closest_item.can_have_label() {
            //     let change_label_item = MenuItem::new();
            //     change_label_item.set_label("Change label");
            //     change_label_item.connect_activate({
            //         let state = Weak::clone(&state.this);
            //         move |_change_label_item| {
            //             if let Some(state) = state.upgrade() {
            //                 let label_entry = gtk::Entry::with_buffer(&gtk::EntryBuffer::new(
            //                     closest_item.get_label(&state.borrow()).unwrap(),
            //                 ));
            //                 let label_dialog = gtk::Dialog::with_buttons(
            //                     Some(&format!("Changing label of {closest_item:?}")),
            //                     Some(&state.borrow().window),
            //                     gtk::DialogFlags::MODAL,
            //                     &[
            //                         ("Ok", gtk::ResponseType::Ok),
            //                         ("Cancel", gtk::ResponseType::Cancel),
            //                     ],
            //                 );
            //                 label_dialog.content_area().add(&label_entry);
            //                 label_dialog.show_all();
            //                 // Don't have a borrow of `state` while the dialog is running,
            //                 // since it runs a main loop which may access state
            //                 let response = label_dialog.run();
            //                 label_dialog.hide();
            //                 let mut state = state.borrow_mut();
            //                 match response {
            //                     gtk::ResponseType::Accept
            //                     | gtk::ResponseType::Ok
            //                     | gtk::ResponseType::Yes
            //                     | gtk::ResponseType::Apply => {
            //                         closest_item
            //                             .replace_label(
            //                                 &mut state,
            //                                 label_entry.buffer().text().into(),
            //                             )
            //                             .unwrap();
            //                     }
            //                     _ => {
            //                         // Don't change the label
            //                     }
            //                 }
            //                 // closest_item.replace_label(&mut state, new_label);
            //                 state.queue_draw();
            //             }
            //         }
            //     });
            //     attach(&change_label_item);
            // }

            eprintln!("TODO: don't show \"make (a)symmetric on initial/terminal handles\"");
            if matches!(
                closest_item,
                ManipulableItem::EdgeMiddleControlPoint { .. } | ManipulableItem::EdgeHandle { .. }
            ) {
                let make_asymmetric_item = MenuItem::new();
                make_asymmetric_item.set_label("Make asymmetric");
                make_asymmetric_item.connect_activate({
                    let state = Weak::clone(&state.this);
                    move |_make_asymmetric_item| {
                        if let Some(state) = state.upgrade() {
                            let mut state = state.borrow_mut();
                            closest_item.make_asymmetric(&mut state).unwrap();
                            state.queue_draw();
                        }
                    }
                });
                attach(&make_asymmetric_item);

                let make_symmetric_item = MenuItem::new();
                make_symmetric_item.set_label("Make symmetric");
                make_symmetric_item.connect_activate({
                    let state = Weak::clone(&state.this);
                    move |_make_symmetric_item| {
                        if let Some(state) = state.upgrade() {
                            let mut state = state.borrow_mut();
                            closest_item.make_symmetric(&mut state).unwrap();
                            state.queue_draw();
                        }
                    }
                });
                attach(&make_symmetric_item);
            }

            menu.show_all();
            menu.popup_at_pointer(Some(press));
        } else {
            eprintln!("TODO: Handle right-clicking on empty canvas");
        }
    }

    fn on_drag(self: Rc<Self>, _: &mut ApplicationState, _: &EventMotion) {
        // do nothing
    }

    fn on_release(self: Rc<Self>, _: &mut ApplicationState, _: &EventButton) {
        // do nothing
    }

    fn cleanup(self: Rc<Self>, _: &mut ApplicationState) {
        // do nothing
    }
}
