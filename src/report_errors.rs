use anyhow::Ok;
use ariadne::{Color, Label, Report, Source};

use crate::parse::{Message, MessageLevel, Messages};

pub fn report_message(message: &Message, file_path: &str) -> anyhow::Result<()> {
    let out = Color::Fixed(81);

    let source = std::fs::read_to_string(file_path)?;
    let offset = message.location.start..message.location.end;
    let kind = match message.level {
        MessageLevel::Error => ariadne::ReportKind::Error,
        MessageLevel::Warning => ariadne::ReportKind::Warning,
    };
    Report::build(kind, file_path, message.location.start)
        .with_message(&message.message)
        .with_label(
            Label::new((file_path, offset))
                .with_message("here")
                .with_color(out),
        )
        .finish()
        .print((file_path, Source::from(source)))?;

    Ok(())
}

pub fn report_errors(messages: &Messages, file_path: &str) -> anyhow::Result<()> {
    for warning in messages.warnings.iter() {
        report_message(warning, file_path)?;
    }

    for error in messages.errors.iter() {
        report_message(error, file_path)?;
    }

    Ok(())
}
