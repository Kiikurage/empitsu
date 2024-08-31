pub trait AsU8Slice {
    fn as_u8_slice(&self) -> &[u8];
}

impl<T> AsU8Slice for T {
    fn as_u8_slice(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts((self as *const T) as *const u8, size_of::<T>())
        }
    }
}

pub trait ParseAs {
    fn parse_as<T>(&self, index: usize) -> &T;
}

impl ParseAs for Vec<u8> {
    fn parse_as<T>(&self, index: usize) -> &T {
        unsafe { &(*(self[index..index+size_of::<T>()].as_ptr() as *const T)) }
    }
}
