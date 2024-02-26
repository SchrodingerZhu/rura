use std::collections::HashMap;

use crate::QualifiedName;

use crate::types::LirType;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct TypeInfo {
    data: HashMap<QualifiedName, HashMap<usize, LirType>>,
}

// there is no need to care about scope as previous pass has already done it
struct Engine {
    type_info: TypeInfo,
}
