use std::alloc::GlobalAlloc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct AllocStats;

pub static ALLOCS: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for AllocStats {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        ALLOCS.fetch_add(layout.size(), Ordering::Relaxed);
        std::alloc::System.alloc(layout)
        // wee_alloc::WeeAlloc::INIT.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        std::alloc::System.dealloc(ptr, layout)
        // wee_alloc::WeeAlloc::INIT.dealloc(ptr, layout)
    }
}
