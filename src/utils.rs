pub fn report(line: usize, location: String, message: String) {
    eprintln!("[line {line}]: Error {location}: {message}")
}

pub fn error(line: usize, message: String) {
    report(line, String::from(""), message)
}