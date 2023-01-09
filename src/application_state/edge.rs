use std::borrow::{BorrowMut, Cow};

use crate::position::{Offset, Position};

#[derive(Debug)]
enum WhichHandle {
    In,
    Out,
}

#[derive(Debug)]
enum HandleKind {
    /// Bezier symmetric point.
    Symmetric {
        in_offset: Offset,
        out_offset: Offset,
        modified_offset: Option<WhichHandle>,
    },
    /// Bezier asymmetric point.
    Asymmetric {
        in_offset: Offset,
        out_offset: Offset,
    },
    /// Bezier initial point. Either the incoming side is a linear segment, or this is the beginning of the edge.
    Initial { out_offset: Offset },
    /// Bezier terminal point. Either the outgoing side is a linear segment, or this is the end of the edge.
    Terminal { in_offset: Offset },
    /// Both incoming and outgoing segments are linear (or off the end of the edge).
    BothLinear,
}

#[derive(Debug)]
pub struct Handle<'a> {
    edge: &'a mut Edge,
    /// control_point_idx = 0 means the initial node
    /// control_point_idx = control_points.len()+1 means the terminal node
    /// otherwise, control_points[control_point_idx-1]
    control_point_idx: usize,
    position: Position,
    kind: HandleKind,
}

impl<'a> Drop for Handle<'a> {
    fn drop(&mut self) {
        let (in_offset, out_offset) = match self.kind {
            HandleKind::Symmetric {
                in_offset,
                out_offset,
                modified_offset: None,
            } => (Some(in_offset), Some(out_offset)),
            HandleKind::Symmetric {
                in_offset,
                modified_offset: Some(WhichHandle::In),
                ..
            } => (Some(in_offset), Some(-in_offset)),
            HandleKind::Symmetric {
                out_offset,
                modified_offset: Some(WhichHandle::Out),
                ..
            } => (Some(-out_offset), Some(out_offset)),
            HandleKind::Asymmetric {
                in_offset,
                out_offset,
            } => (Some(in_offset), Some(out_offset)),
            HandleKind::Initial { out_offset } => (None, Some(out_offset)),
            HandleKind::Terminal { in_offset } => (Some(in_offset), None),
            HandleKind::BothLinear => (None, None),
        };
        // TODO: handle kind.modified_offset
        if self.control_point_idx == 0 {
            debug_assert!(in_offset.is_none(), "initial edge has no in_offset");
            match (out_offset, self.edge.segments.first_mut().unwrap()) {
                (None, EdgeSegment::Linear) => {},
                (None, EdgeSegment::Bezier(_))|(Some(_), EdgeSegment::Linear) => unreachable!("initial edge has out_offset if and only if the first segment is a bezier segment"),
                (Some(new_out_offset), EdgeSegment::Bezier(segment)) => {
                    segment.from_offset = new_out_offset;
                },
            }
        } else if self.control_point_idx == self.edge.control_points.len() + 1 {
            debug_assert!(out_offset.is_none(), "terminal edge has no out_offset");
            match (in_offset, self.edge.segments.last_mut().unwrap()) {
                (None, EdgeSegment::Linear) => {},
                (None, EdgeSegment::Bezier(_))|(Some(_), EdgeSegment::Linear) => unreachable!("terminal edge has in_offset if and only if the last segment is a bezier segment"),
                (Some(new_in_offset), EdgeSegment::Bezier(segment)) => {
                    segment.to_offset = new_in_offset;
                },
            }
        } else {
            let [in_segment, out_segment]: &mut [EdgeSegment; 2] = self.edge.segments
                [self.control_point_idx - 1..=self.control_point_idx]
                .borrow_mut()
                .try_into()
                .unwrap();
            match (out_offset, out_segment) {
                    (None, EdgeSegment::Linear) => {},
                    (None, EdgeSegment::Bezier(_))|(Some(_), EdgeSegment::Linear) => unreachable!("initial edge has out_offset if and only if the first segment is a bezier segment"),
                    (Some(new_out_offset), EdgeSegment::Bezier(segment)) => {
                        segment.from_offset = new_out_offset;
                    },
                }
            match (in_offset, in_segment) {
                    (None, EdgeSegment::Linear) => {},
                    (None, EdgeSegment::Bezier(_))|(Some(_), EdgeSegment::Linear) => unreachable!("terminal edge has in_offset if and only if the last segment is a bezier segment"),
                    (Some(new_in_offset), EdgeSegment::Bezier(segment)) => {
                        segment.to_offset = new_in_offset;
                    },
                }
            self.edge.control_points[self.control_point_idx - 1] = self.position;
        };
        // match (out_offset, out_segment) {
        //     (None, EdgeSegment::Linear) => {}
        //     (None, EdgeSegment::Bezier(_)) | (Some(_), EdgeSegment::Linear) => unreachable!(
        //         "initial edge has out_offset if and only if the first segment is a bezier segment"
        //     ),
        //     (Some(new_out_offset), EdgeSegment::Bezier(segment)) => {
        //         segment.from_offset = new_out_offset;
        //     }
        // }
        // match (in_offset, in_segment) {
        //     (None, EdgeSegment::Linear) => {}
        //     (None, EdgeSegment::Bezier(_)) | (Some(_), EdgeSegment::Linear) => unreachable!(
        //         "terminal edge has in_offset if and only if the last segment is a bezier segment"
        //     ),
        //     (Some(new_in_offset), EdgeSegment::Bezier(segment)) => {
        //         segment.to_offset = new_in_offset;
        //     }
        // }
    }
}

impl<'a> Handle<'a> {
    pub fn position(&self) -> &Position {
        &self.position
    }
    pub fn in_offset(&mut self) -> Result<&mut Offset, ManipulateEdgeError> {
        match &mut self.kind {
            HandleKind::Symmetric {
                in_offset,
                modified_offset,
                ..
            } => {
                *modified_offset = Some(WhichHandle::In);
                Ok(in_offset)
            }
            HandleKind::Asymmetric { in_offset, .. } | HandleKind::Terminal { in_offset } => {
                Ok(in_offset)
            }
            HandleKind::Initial { .. } => Err(ManipulateEdgeError::InvalidEdgeSegment {
                segment_idx: usize::MAX,
            }),
            HandleKind::BothLinear => Err(ManipulateEdgeError::EdgeSegmentNotBezier {
                segment_idx: self.control_point_idx - 1,
            }),
        }
    }
    pub fn out_offset(&mut self) -> Result<&mut Offset, ManipulateEdgeError> {
        match &mut self.kind {
            HandleKind::Symmetric {
                out_offset,
                modified_offset,
                ..
            } => {
                *modified_offset = Some(WhichHandle::Out);
                Ok(out_offset)
            }
            HandleKind::Asymmetric { out_offset, .. } | HandleKind::Initial { out_offset } => {
                Ok(out_offset)
            }
            HandleKind::Terminal { .. } => Err(ManipulateEdgeError::InvalidEdgeSegment {
                segment_idx: self.control_point_idx,
            }),
            HandleKind::BothLinear => Err(ManipulateEdgeError::EdgeSegmentNotBezier {
                segment_idx: self.control_point_idx,
            }),
        }
    }
}

// #[derive(Clone, Debug)]
// pub struct BezierEdge {
//     pub from_offset: Offset,
//     pub mid_handles: Vec<Handle>,
//     pub to_offset: Offset,
// }

// #[derive(Clone, Debug)]
// pub enum EdgeKind {
//     Bezier(BezierEdge),
//     Linear,
// }

#[derive(Clone, Debug)]
pub struct BezierEdgeSegment {
    pub from_offset: Offset,
    pub to_offset: Offset,
}

#[derive(Clone, Debug)]
pub enum EdgeSegment {
    Linear,
    Bezier(BezierEdgeSegment),
}

impl EdgeSegment {
    pub fn as_bezier_edge_segment(&self) -> Option<&BezierEdgeSegment> {
        match self {
            EdgeSegment::Linear => None,
            EdgeSegment::Bezier(segment) => Some(segment),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Edge {
    pub label: Cow<'static, str>,
    // Offset from initial node
    pub start_offset: Offset,
    // Offset from terminal node
    pub end_offset: Offset,
    // Intermediate locations
    pub control_points: Vec<Position>,
    // Intermediate edge segments
    pub segments: Vec<EdgeSegment>,
    // Symmetric bezier handles. If an index `n` is in this, then the `n`th EdgeSegment's to_handle and
    // the `n+1`th EdgeSegment's from_handle should be kept exactly opposite one another.
    pub symmetries: bit_set::BitSet,
}

impl Default for Edge {
    fn default() -> Self {
        Self {
            label: Cow::Borrowed(""),
            start_offset: Default::default(),
            end_offset: Default::default(),
            control_points: Default::default(),
            segments: vec![EdgeSegment::Linear],
            symmetries: Default::default(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ManipulateEdgeError {
    EdgeSegmentNotBezier {
        segment_idx: usize,
    },
    EdgeSegmentNotLinear {
        segment_idx: usize,
    },
    InvalidEdgeSegment {
        segment_idx: usize,
    },
    InvalidControlPoint {
        control_point_idx: usize,
    },
    /// An operation that required an internal (not initial or terminal) control point
    /// was given an initial or terminal control point.
    ControlPointNotInternal {
        control_point_idx: usize,
    },
}

impl Edge {
    pub fn handle(
        &mut self,
        initial_node: &Position,
        terminal_node: &Position,
        control_point_idx: usize,
    ) -> Option<Handle<'_>> {
        let kind = if control_point_idx == 0 {
            HandleKind::Initial {
                out_offset: self.start_offset,
            }
        } else if control_point_idx == self.control_points.len() + 1 {
            HandleKind::Terminal {
                in_offset: self.end_offset,
            }
        } else if control_point_idx > self.control_points.len() + 1 {
            return None;
        } else {
            let [a, b]: &[EdgeSegment; 2] = self.segments
                [control_point_idx - 1..=control_point_idx]
                .try_into()
                .unwrap();
            if self.symmetries.contains(control_point_idx) {
                HandleKind::Symmetric {
                    in_offset: a.as_bezier_edge_segment().unwrap().to_offset,
                    out_offset: b.as_bezier_edge_segment().unwrap().from_offset,
                    modified_offset: None,
                }
            } else {
                match (a, b) {
                    (EdgeSegment::Linear, EdgeSegment::Linear) => HandleKind::BothLinear,
                    (EdgeSegment::Linear, EdgeSegment::Bezier(outgoing)) => HandleKind::Initial {
                        out_offset: outgoing.from_offset,
                    },
                    (EdgeSegment::Bezier(incoming), EdgeSegment::Linear) => HandleKind::Terminal {
                        in_offset: incoming.to_offset,
                    },
                    (EdgeSegment::Bezier(incoming), EdgeSegment::Bezier(outgoing)) => {
                        HandleKind::Asymmetric {
                            in_offset: incoming.to_offset,
                            out_offset: outgoing.from_offset,
                        }
                    }
                }
            }
        };
        let position = *Self::control_point(
            &self.control_points,
            initial_node,
            terminal_node,
            control_point_idx,
        )
        .expect("already checked indices");
        Some(Handle {
            edge: self,
            control_point_idx,
            position,
            kind,
        })
    }

    pub fn control_point<'a>(
        control_points: &'a [Position],
        initial_node: &'a Position,
        terminal_node: &'a Position,
        control_point_idx: usize,
    ) -> Result<&'a Position, ManipulateEdgeError> {
        if control_point_idx == 0 {
            Ok(initial_node)
        } else if let Some(control_point) = control_points.get(control_point_idx - 1) {
            Ok(control_point)
        } else if control_point_idx - 1 == control_points.len() {
            Ok(terminal_node)
        } else {
            Err(ManipulateEdgeError::InvalidControlPoint { control_point_idx })
        }
    }

    pub fn make_bezier(
        &mut self,
        initial_node: &Position,
        terminal_node: &Position,
        segment_idx: usize,
    ) -> Result<(), ManipulateEdgeError> {
        match self.segments.get_mut(segment_idx) {
            Some(segment @ EdgeSegment::Linear) => {
                let start_position = Self::control_point(
                    &self.control_points,
                    initial_node,
                    terminal_node,
                    segment_idx,
                )
                .expect("already checked the index");
                let end_position = Self::control_point(
                    &self.control_points,
                    initial_node,
                    terminal_node,
                    segment_idx + 1,
                )
                .expect("already checked the index");
                let to_offset = (start_position - end_position) / 3.0;
                let from_offset = -to_offset;
                *segment = EdgeSegment::Bezier(BezierEdgeSegment {
                    from_offset,
                    to_offset,
                });
                Ok(())
            }
            Some(EdgeSegment::Bezier(_)) => Ok(()),
            None => Err(ManipulateEdgeError::InvalidEdgeSegment { segment_idx }),
        }
    }

    /// Make a control point symmetric.
    ///
    /// The previous offsets will be "averaged" the relevant offsets to
    /// produce a smooth transition close to what existed previously.
    ///
    /// This function will fail if called on the initial control point (which has no incoming segment
    /// to pair with), the terminal control point (which has no outgoing segment to pair with),
    /// or an invalid control point.
    pub fn make_symmetric(
        &mut self,
        initial_node: &Position,
        terminal_node: &Position,
        control_point_idx: usize,
    ) -> Result<(), ManipulateEdgeError> {
        if control_point_idx == 0 || control_point_idx - 1 == self.control_points.len() {
            return Err(ManipulateEdgeError::ControlPointNotInternal { control_point_idx });
        } else if control_point_idx - 1 > self.control_points.len() {
            return Err(ManipulateEdgeError::InvalidControlPoint { control_point_idx });
        }
        self.make_bezier(initial_node, terminal_node, control_point_idx - 1)
            .expect("already checked index");
        self.make_bezier(initial_node, terminal_node, control_point_idx)
            .expect("already checked index");
        let [incoming_segment, outgoing_segment]: &mut [EdgeSegment; 2] = self.segments
            [control_point_idx - 1..=control_point_idx]
            .borrow_mut()
            .try_into()
            .unwrap();
        match (incoming_segment, outgoing_segment) {
            (EdgeSegment::Bezier(incoming_segment), EdgeSegment::Bezier(outgoing_segment)) => {
                let offset = (incoming_segment.to_offset - outgoing_segment.from_offset) / 2.0;
                incoming_segment.to_offset = offset;
                outgoing_segment.from_offset = -offset;
            }
            _ => unreachable!("called make_bezier"),
        }
        self.symmetries.insert(control_point_idx);
        Ok(())
    }

    /// Make a control point asymmetric.
    ///
    /// This function will fail if called on the initial control point (which has no incoming segment
    /// to pair with), the terminal control point (which has no outgoing segment to pair with),
    /// or an invalid control point.
    pub fn make_asymmetric(
        &mut self,
        initial_node: &Position,
        terminal_node: &Position,
        control_point_idx: usize,
    ) -> Result<(), ManipulateEdgeError> {
        if control_point_idx == 0 || control_point_idx - 1 == self.control_points.len() {
            return Err(ManipulateEdgeError::ControlPointNotInternal { control_point_idx });
        } else if control_point_idx - 1 > self.control_points.len() {
            return Err(ManipulateEdgeError::InvalidControlPoint { control_point_idx });
        }
        self.make_bezier(initial_node, terminal_node, control_point_idx - 1)
            .expect("already checked index");
        self.make_bezier(initial_node, terminal_node, control_point_idx)
            .expect("already checked index");

        self.symmetries.remove(control_point_idx);
        Ok(())
    }

    /// Make a segment linear
    pub fn make_linear(&mut self, segment_idx: usize) -> Result<(), ManipulateEdgeError> {
        match self.segments.get_mut(segment_idx) {
            Some(segment @ EdgeSegment::Bezier(_)) => {
                if segment_idx > 0 {
                    self.symmetries.remove(segment_idx - 1);
                }
                self.symmetries.remove(segment_idx);
                *segment = EdgeSegment::Linear;
                Ok(())
            }
            Some(EdgeSegment::Linear) => Ok(()),
            None => Err(ManipulateEdgeError::InvalidEdgeSegment { segment_idx }),
        }
    }

    #[cfg(any())]
    /// control_point_idx = 0 means the initial node
    /// control_point_idx = control_points.len()+1 means the terminal node
    /// otherwise, control_points[control_point_idx-1]
    pub fn move_handle(
        &mut self,
        initial_node: &Position,
        terminal_node: &Position,
        control_point_idx: usize,
        out_handle: bool,
        destination: Position,
    ) -> Result<(), ManipulateEdgeError> {
        self.move_handle_with_optional_symmetry(
            initial_node,
            terminal_node,
            control_point_idx,
            out_handle,
            destination,
            true,
        )
    }
    #[cfg(any())]
    /// control_point_idx = 0 means the initial node
    /// control_point_idx = control_points.len()+1 means the terminal node
    /// otherwise, control_points[control_point_idx-1]
    pub fn move_handle_with_optional_symmetry(
        &mut self,
        initial_node: &Position,
        terminal_node: &Position,
        control_point_idx: usize,
        out_handle: bool,
        destination: Position,
        symmetry: bool,
    ) -> Result<(), ManipulateEdgeError> {
        let control_point: Position = if control_point_idx == 0 {
            if !out_handle {
                unreachable!("Initial node of an edge has no in-handle")
            }
            initial_node + self.start_offset
        } else if control_point_idx == 1 + self.control_points.len() {
            if out_handle {
                unreachable!("Terminal node of an edge has no out-handle")
            }
            terminal_node + self.end_offset
        } else {
            self.control_points[control_point_idx - 1]
        };
        // out-handle for nth point is in nth segment
        // in-handle for nth point is in (n-1)th segment
        let segment_idx = control_point_idx - (out_handle as usize);

        let offset = destination - control_point;
        let segment = &mut self.segments[segment_idx];
        use EdgeSegment::*;
        match segment {
            Linear => Err(ManipulateEdgeError::EdgeSegmentNotBezier { segment_idx }),
            Bezier(segment) => {
                if out_handle {
                    if symmetry {
                        // update from_handle of next segment
                        let segment = &mut self.segments[segment_idx + 1];
                        match segment {
                            Linear => Err(ManipulateEdgeError::EdgeSegmentNotBezier {
                                segment_idx: segment_idx + 1,
                            })?,
                            Bezier(segment) => {
                                segment.from_offset = -offset;
                            }
                        };
                    }
                    // move to_handle of segment
                    segment.to_offset = offset;
                    Ok(())
                } else {
                    if symmetry {
                        // update to_handle of previous segment
                        let segment = &mut self.segments[segment_idx - 1];
                        match segment {
                            Linear => Err(ManipulateEdgeError::EdgeSegmentNotBezier {
                                segment_idx: segment_idx - 1,
                            })?,
                            Bezier(segment) => {
                                segment.to_offset = -offset;
                            }
                        };
                    }
                    // move from_handle of segment
                    segment.from_offset = offset;
                    Ok(())
                }
            }
        }
    }
}
