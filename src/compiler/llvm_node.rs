use parsing::ast::BinaryOperator;

#[derive(Clone, Debug)]
pub enum LLVMNode {
    Define{
        name: String,
        return_type: String,
        args: Vec<String>,
        basic_blocks: Vec<LLVMNode>,
    },
    BinaryOp{
        op: BinaryOperator,
        return_type: String,
        l: String,
        r: String,
        dst: String,
    },
    Ret{
        ty: String,
        val: String,
    },
    BasicBlock{
        label: String,
        instructions: Vec<LLVMNode>,
    },
    Unreachable,
    ConditionalBranch{
        condition: String,
        t: String,
        f: String,
    },
    Branch {
        branch: String,
    },
    Alloca {
        ty: String,
        ptr: String,
    },
    Store {
        ty: String,
        ptr: String,
        source_val: String,
    },
    Load {
        ty: String,
        dst: String,
        ptr: String,
    },
    Type {
        name: String,
        types: Vec<String>
    },
}

impl LLVMNode {
    pub fn push_to_basic_block(&mut self, node: LLVMNode) {
        match self {
            &mut LLVMNode::BasicBlock {label: _, ref mut instructions} => {
                instructions.push(node)
            },
            _ => panic!("not basic block")
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            &LLVMNode::BasicBlock {ref label, instructions: _} => {
                label.clone()
            },
            _ => panic!("not basic block")
        }
    }

    pub fn get_instruction(&self) -> Vec<LLVMNode> {
        match self {
            &LLVMNode::BasicBlock {label: _, ref instructions} => {
                instructions.clone()
            },
            _ => panic!("not basic block")
        }
    }

    pub fn append_to_basic_block(&mut self, mut node: Vec<LLVMNode>) {
        match self {
            &mut LLVMNode::BasicBlock {label: _, ref mut instructions} => {
                instructions.append(&mut node)
            },
            _ => panic!("not basic block")
        }
    }
}