#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Position {
    pub x: f64,
    pub y: f64,
}

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Offset {
    pub x: f64,
    pub y: f64,
}

impl Offset {
    pub fn norm(&self) -> f64 {
        self.x * self.x + self.y * self.y
    }
}

impl From<(f64, f64)> for Position {
    fn from((x, y): (f64, f64)) -> Self {
        Self { x, y }
    }
}

impl From<Position> for (f64, f64) {
    fn from(Position { x, y }: Position) -> Self {
        (x, y)
    }
}

impl From<(f64, f64)> for Offset {
    fn from((x, y): (f64, f64)) -> Self {
        Self { x, y }
    }
}

impl From<Offset> for (f64, f64) {
    fn from(Offset { x, y }: Offset) -> Self {
        (x, y)
    }
}

macro_rules! impl_op {
    ($op_trait:ident, $op_fn:ident, $rhs:ident $op:tt $lhs:ident => $output:ident) => {
        impl $op_trait<$rhs> for $lhs {
            type Output = $output;
            fn $op_fn(self, rhs: $rhs) -> Self::Output {
                $output { x: self.x $op rhs.x, y: self.y $op rhs.y }
            }
        }
        impl $op_trait<$rhs> for &$lhs {
            type Output = $output;
            fn $op_fn(self, rhs: $rhs) -> Self::Output {
                $output { x: self.x $op rhs.x, y: self.y $op rhs.y }
            }
        }
        impl $op_trait<&$rhs> for $lhs {
            type Output = $output;
            fn $op_fn(self, rhs: &$rhs) -> Self::Output {
                $output { x: self.x $op rhs.x, y: self.y $op rhs.y }
            }
        }
        impl $op_trait<&$rhs> for &$lhs {
            type Output = $output;
            fn $op_fn(self, rhs: &$rhs) -> Self::Output {
                $output { x: self.x $op rhs.x, y: self.y $op rhs.y }
            }
        }
    };
    ($op_trait:ident, $op_fn:ident, -($rhs:ident $op:tt $lhs:ident) => $output:ident) => {
        impl $op_trait<$rhs> for $lhs {
            type Output = $output;
            fn $op_fn(self, rhs: $rhs) -> Self::Output {
                $output { x: -(self.x $op rhs.x), y: -(self.y $op rhs.y) }
            }
        }
        impl $op_trait<&$rhs> for $lhs {
            type Output = $output;
            fn $op_fn(self, rhs: &$rhs) -> Self::Output {
                $output { x: -(self.x $op rhs.x), y: -(self.y $op rhs.y) }
            }
        }
        impl $op_trait<$rhs> for &$lhs {
            type Output = $output;
            fn $op_fn(self, rhs: $rhs) -> Self::Output {
                $output { x: -(self.x $op rhs.x), y: -(self.y $op rhs.y) }
            }
        }
        impl $op_trait<&$rhs> for &$lhs {
            type Output = $output;
            fn $op_fn(self, rhs: &$rhs) -> Self::Output {
                $output { x: -(self.x $op rhs.x), y: -(self.y $op rhs.y) }
            }
        }
    };
}

use std::ops::{Add, Div, Mul, Neg, Sub};

impl_op!(Add, add, Position + Offset => Position);
impl_op!(Add, add, Offset + Position => Position);
impl_op!(Add, add, Offset + Offset => Offset);

impl_op!(Sub, sub, Position - Offset => Position);
impl_op!(Sub, sub, -(Offset - Position) => Position);
impl_op!(Sub, sub, Offset - Offset => Offset);
impl_op!(Sub, sub, Position - Position => Offset);

impl Neg for Offset {
    type Output = Offset;
    fn neg(self) -> Self {
        Offset {
            x: -self.x,
            y: -self.y,
        }
    }
}
impl Neg for &Offset {
    type Output = Offset;
    fn neg(self) -> Offset {
        Offset {
            x: -self.x,
            y: -self.y,
        }
    }
}

impl Mul<f64> for Offset {
    type Output = Offset;
    fn mul(self, rhs: f64) -> Self::Output {
        Offset {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}
impl Mul<f64> for &Offset {
    type Output = Offset;
    fn mul(self, rhs: f64) -> Self::Output {
        Offset {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl Mul<Offset> for f64 {
    type Output = Offset;
    fn mul(self, rhs: Offset) -> Self::Output {
        Offset {
            x: self * rhs.x,
            y: self * rhs.y,
        }
    }
}
impl Mul<&Offset> for f64 {
    type Output = Offset;
    fn mul(self, rhs: &Offset) -> Self::Output {
        Offset {
            x: self * rhs.x,
            y: self * rhs.y,
        }
    }
}

impl Div<f64> for Offset {
    type Output = Offset;
    fn div(self, rhs: f64) -> Self::Output {
        Offset {
            x: self.x / rhs,
            y: self.y / rhs,
        }
    }
}
impl Div<f64> for &Offset {
    type Output = Offset;
    fn div(self, rhs: f64) -> Self::Output {
        Offset {
            x: self.x / rhs,
            y: self.y / rhs,
        }
    }
}

macro_rules! make_cairo_context_ext_trait {
    (
        $trait_name:ident for $type_name:ty;
        $( $(($pos_count:tt))? $method:ident ($($other_arg:ident : $other_ty:ty),* ) => $actual_method:ident );* ;
    ) => {
        pub trait $trait_name {
            $(
                make_cairo_context_ext_trait!{@trait fn $(($pos_count))? $method ($($other_arg : $other_ty),* ) => $actual_method}
            )*
        }
        impl $trait_name for $type_name {
            $(
                make_cairo_context_ext_trait!{@impl fn $(($pos_count))? $method ($($other_arg : $other_ty),* ) => $actual_method}
            )*
        }
    };
    (@trait fn $((1))? $method:ident($($other_arg:ident : $other_ty:ty),* ) => $actual_method:ident ) => {
        fn $method(&self, pos: Position $(, $other_arg: $other_ty)*);
    };
    (@impl fn $((1))? $method:ident($($other_arg:ident : $other_ty:ty),* ) => $actual_method:ident ) => {
        fn $method(&self, pos: Position $(, $other_arg: $other_ty)*){
            self.$actual_method(pos.x, pos.y $(, $other_arg)*)
        }
    };
    (@trait fn $((3))? $method:ident($($other_arg:ident : $other_ty:ty),* ) => $actual_method:ident ) => {
        fn $method(&self, p1: Position, p2: Position, p3: Position $(, $other_arg: $other_ty)*);
    };
    (@impl fn $((3))? $method:ident($($other_arg:ident : $other_ty:ty),* ) => $actual_method:ident ) => {
        fn $method(&self, p1: Position, p2: Position, p3: Position $(, $other_arg: $other_ty)*){
            self.$actual_method(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y $(, $other_arg)*)
        }
    };
}

use gtk::cairo::Context;

make_cairo_context_ext_trait! {
    CairoContextExt for Context;
    (1) move_to_pos() => move_to;
    (1) line_to_pos() => line_to;
    (1) arc_pos(radius: f64, angle1: f64, angle2: f64) => arc;
    (3) curve_to_pos() => curve_to;
}
