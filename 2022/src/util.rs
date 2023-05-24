
pub fn read_input(file_path: &str) -> std::io::Result<String> {
    std::fs::read_to_string(format!("inputs/{file_path}"))
}
