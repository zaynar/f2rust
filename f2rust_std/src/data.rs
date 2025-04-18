#[derive(Clone)]
pub enum Val<'a> {
    I(i32),
    R(f32),
    D(f64),
    L(bool),
    C(&'a [u8]),
}

impl<'a> Val<'a> {
    pub fn into_i32(self) -> i32 {
        match self {
            Val::I(n) => n,
            Val::R(n) => n as i32,
            Val::D(n) => n as i32,
            _ => panic!("type mismatch in DATA"),
        }
    }

    pub fn into_f32(self) -> f32 {
        match self {
            Val::I(n) => n as f32,
            Val::R(n) => n,
            Val::D(n) => n as f32,
            _ => panic!("type mismatch in DATA"),
        }
    }

    pub fn into_f64(self) -> f64 {
        match self {
            Val::I(n) => n as f64,
            Val::R(n) => n as f64,
            Val::D(n) => n,
            _ => panic!("type mismatch in DATA"),
        }
    }

    pub fn into_bool(self) -> bool {
        match self {
            Val::L(n) => n,
            _ => panic!("type mismatch in DATA"),
        }
    }

    pub fn into_str(self) -> &'a [u8] {
        match self {
            Val::C(n) => n,
            _ => panic!("type mismatch in DATA"),
        }
    }
}
