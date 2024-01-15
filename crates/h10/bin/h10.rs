use h10::alloc::ALLOCS;
use h10::type_check_module;
use h10::utils::comma_sep;

fn main() {
    let (ty_kinds, bind_tys) = type_check_module(&std::fs::read_to_string("Prelude.hs").unwrap());

    for (ty, kind) in ty_kinds {
        println!("{} : {}", ty, kind);
    }

    println!();

    for (value, scheme) in bind_tys.iter() {
        println!("{} : {}", value, scheme);
    }

    println!(
        "Total allocs: {} bytes.",
        comma_sep(&ALLOCS.load(std::sync::atomic::Ordering::SeqCst).to_string()),
    );
}

/*
use lexgen_util::Loc;

struct LocDisplay(Loc);

impl std::fmt::Debug for LocDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0.line, self.0.col)
    }
}
*/
