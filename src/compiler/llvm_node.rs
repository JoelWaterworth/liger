use parsing::ast::BinaryOperator;

#[derive(Clone, Debug)]
pub enum LLVMNode {
    Define{
        name: String,
        return_type: String,
        args: Vec<String>,
        basic_blocks: Vec<LLVMNode>,
    },
    Declare {
        name: String,
        return_type: String,
        args: Vec<String>,
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
        terminator: Box<LLVMNode>,
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
    GetElementptr {
        ty: String,
        src: String,
        dst: String,
        offset: Vec<u8>,
    },
    ExtractValue {
        ty: String,
        src: String,
        dst: String,
        offset: u8,
    },
    Call {
        dst: String,
        ret_ty: String,
        func: String,
        args: Vec<(String, String)>,
    },
}

impl LLVMNode {
    pub fn push_to_basic_block(&mut self, node: LLVMNode) {
        match self {
            &mut LLVMNode::BasicBlock {label: _, ref mut instructions, terminator: _} => {
                instructions.push(node)
            },
            _ => panic!("not basic block")
        }
    }
    pub fn basic_block_terminate(&mut self, node: LLVMNode) {
        match self {
            &mut LLVMNode::BasicBlock {label: _, instructions: _, ref mut terminator} => {
                *terminator = Box::new(node);
            },
            _ => panic!("not basic block")
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            &LLVMNode::BasicBlock {ref label, instructions: _, terminator: _} => {
                label.clone()
            },
            _ => panic!("not basic block")
        }
    }

    pub fn get_terminator(&self) -> LLVMNode {
        match self {
            &LLVMNode::BasicBlock {label: _, instructions: _, ref terminator} => {
                terminator.as_ref().clone()
            },
            _ => panic!("not basic block")
        }
    }

    pub fn get_instruction(&self) -> Vec<LLVMNode> {
        match self {
            &LLVMNode::BasicBlock {label: _, ref instructions, terminator: _} => {
                instructions.clone()
            },
            _ => panic!("not basic block")
        }
    }

    #[allow(dead_code)]
    pub fn append_to_basic_block(&mut self, mut node: Vec<LLVMNode>) {
        match self {
            &mut LLVMNode::BasicBlock {label: _, ref mut instructions, terminator: _} => {
                instructions.append(&mut node)
            },
            _ => panic!("not basic block")
        }
    }
}