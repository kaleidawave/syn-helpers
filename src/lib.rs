mod custom_structures;
mod visit_fields;

pub use custom_structures::*;
pub use visit_fields::*;

pub use inflector::cases::snakecase::to_snake_case as str_to_snake_case;
