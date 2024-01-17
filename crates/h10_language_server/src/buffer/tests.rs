use crate::buffer::Buffer;

use tower_lsp::lsp_types::Position;

fn pos(line: u32, char: u32) -> Position {
    Position {
        line,
        character: char,
    }
}

#[test]
fn range_removal_1() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "line 1\n");
    buffer.insert(pos(1, 0), "line 2\n");
    buffer.insert(pos(2, 0), "line 3\n");

    assert_eq!(buffer.len_lines(), 4);

    let mut removed_text = String::new();

    buffer.remove(
        &mut removed_text,
        pos(0, 6), // end of the first line
        pos(1, 7), // end of the second line
    );

    assert_eq!(removed_text, "\nline 2\n");
    assert_eq!(buffer.len_lines(), 2);
    assert_eq!(buffer.line_text(0), "line 1line 3");
    assert_eq!(buffer.line_text(1), "");
}

#[test]
fn range_removal_2() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "line 1\n");
    buffer.insert(pos(1, 0), "line 2\n");
    buffer.insert(pos(2, 0), "line 3\n");

    assert_eq!(buffer.len_lines(), 4);

    let mut removed_text = String::new();

    buffer.remove(
        &mut removed_text,
        pos(0, 6), // end of the first line
        pos(1, 6), // end of the second line, excluding newline
    );

    assert_eq!(removed_text, "\nline 2");
    assert_eq!(buffer.len_lines(), 3);
    assert_eq!(buffer.line_text(0), "line 1");
    assert_eq!(buffer.line_text(1), "line 3");
    assert_eq!(buffer.line_text(2), "");
}

#[test]
fn range_removal_3() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "line 1\n");

    let mut removed_text = String::new();

    buffer.remove(&mut removed_text, pos(0, 0), pos(0, 7));

    assert_eq!(removed_text, "line 1\n");
    assert_eq!(buffer.len_lines(), 1);
    assert_eq!(buffer.line_text(0), "");
}

#[test]
fn range_removal_4() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "line 1\n");

    let mut removed_text = String::new();

    buffer.remove(&mut removed_text, pos(0, 0), pos(0, 6));

    assert_eq!(removed_text, "line 1");
    assert_eq!(buffer.len_lines(), 2);
    assert_eq!(buffer.line_text(0), "");
    assert_eq!(buffer.line_text(0), "");
}

#[test]
fn range_removal_5() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "line 1\n");
    buffer.insert(pos(1, 0), "\n");
    buffer.insert(pos(2, 0), "line 3\n");

    let mut removed_text = String::new();

    buffer.remove(&mut removed_text, pos(0, 0), pos(2, 7));

    assert_eq!(removed_text, "line 1\n\nline 3\n");
    assert_eq!(buffer.len_lines(), 1);
    assert_eq!(buffer.line_text(0), "");
}

#[test]
fn range_removal_6() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "a\n");
    buffer.insert(pos(1, 0), "\n");

    let mut removed_text = String::new();

    buffer.remove(&mut removed_text, pos(0, 0), pos(1, 1));

    assert_eq!(removed_text, "a\n\n");
    assert_eq!(buffer.len_lines(), 1);
    assert_eq!(buffer.line_text(0), "");
}

#[test]
fn range_removal_7() {
    let mut buffer = Buffer::new();

    buffer.insert(pos(0, 0), "line 1\n");
    buffer.insert(pos(1, 0), "\n");
    buffer.insert(pos(2, 0), "line 3\n");

    let mut removed_text = String::new();

    buffer.remove(&mut removed_text, pos(0, 0), pos(2, 1));

    assert_eq!(removed_text, "line 1\n\nl");
    assert_eq!(buffer.len_lines(), 2);
    assert_eq!(buffer.line_text(0), "ine 3");
    assert_eq!(buffer.line_text(1), "");
}
