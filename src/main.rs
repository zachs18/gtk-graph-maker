use gtk::{prelude::*, RadioButton};
use gtk::{ApplicationWindow, Grid};

mod position;

mod application_state;
use application_state::*;

fn build_ui(application: &gtk::Application) {
    let window = ApplicationWindow::new(application);
    // set_visual(&window, None);

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
    grid.attach(&drawing_area, 0, 0, 4, 1);

    let move_tool_button = RadioButton::new();
    move_tool_button.set_label("Move Tool");
    let create_nodes_tool_button = RadioButton::from_widget(&move_tool_button);
    create_nodes_tool_button.set_label("Create Nodes Tool");
    let create_edges_tool_button = RadioButton::from_widget(&move_tool_button);
    create_edges_tool_button.set_label("Create Edges Tool");
    let modify_tool_button = RadioButton::from_widget(&move_tool_button);
    modify_tool_button.set_label("Modify Tool");

    grid.attach(&move_tool_button, 0, 1, 1, 1);
    grid.attach(&create_nodes_tool_button, 1, 1, 1, 1);
    grid.attach(&create_edges_tool_button, 2, 1, 1, 1);
    grid.attach(&modify_tool_button, 3, 1, 1, 1);

    let state = ApplicationState::new(
        window.clone(),
        drawing_area.clone(),
        &move_tool_button,
        &create_nodes_tool_button,
        &create_edges_tool_button,
        &modify_tool_button,
    );
    {
        let mut state = state.borrow_mut();
        let n1 = state.add_node(Node {
            label: Some("Start".into()),
            position: (100.0, 100.0).into(),
        });
        let n2 = state.add_node(Node {
            label: Some("End".into()),
            position: (400.0, 400.0).into(),
        });
        #[cfg(any())]
        let edge = Edge {
            label: "Test".into(),
            kind: EdgeKind::Bezier(BezierEdge {
                from_offset: (0.0, 300.0).into(),
                to_offset: (0.0, -300.0).into(),
                mid_handles: vec![
                    Handle::Symmetric((-50.0, -150.0).into(), (250.0, 250.0).into()),
                    Handle::Asymmetric(
                        (-50.0, -150.0).into(),
                        (200.0, 200.0).into(),
                        (40.0, 40.0).into(),
                    ),
                ],
            }),
        };
        let edge = Edge {
            label: Some("Test".into()),
            start_offset: (0.0, 0.0).into(),
            end_offset: (0.0, 0.0).into(),
            control_points: vec![(250.0, 250.0).into(), (200.0, 200.0).into()],
            segments: vec![
                EdgeSegment::Bezier(BezierEdgeSegment {
                    from_offset: (0.0, 300.0).into(),
                    to_offset: (-50.0, -150.0).into(),
                }),
                EdgeSegment::Bezier(BezierEdgeSegment {
                    from_offset: (50.0, 150.0).into(),
                    to_offset: (-50.0, -150.0).into(),
                }),
                EdgeSegment::Bezier(BezierEdgeSegment {
                    from_offset: (40.0, 40.0).into(),
                    to_offset: (0.0, -300.0).into(),
                }),
            ],
            symmetries: Default::default(),
        };
        state.add_edge(n1, n2, edge);
    }

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
