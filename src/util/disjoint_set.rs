use std::{cell::Cell, mem::replace};

#[derive(Debug, Clone)]
pub struct DisjointForest<N> {
    nodes: Vec<DisjointNode<N>>,
}

impl<N> Default for DisjointForest<N> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
enum DisjointNode<N> {
    Descendant(Cell<u32>),
    Root(N),
}

impl<N> DisjointForest<N> {
    pub const fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, meta: N) -> u32 {
        let idx = u32::try_from(self.nodes.len()).expect("too many nodes in the disjoint forest");
        self.nodes.push(DisjointNode::Root(meta));
        idx
    }

    pub fn find(&self, mut idx: u32) -> (u32, &N) {
        loop {
            match &self.nodes[idx as usize] {
                DisjointNode::Descendant(parent) => {
                    // Traverse to its parent
                    idx = parent.get();

                    // Set its parent to its grandparent
                    parent.set(match &self.nodes[idx as usize] {
                        DisjointNode::Descendant(grandparent) => grandparent.get(),
                        DisjointNode::Root(_) => idx,
                    });
                }
                DisjointNode::Root(data) => break (idx, data),
            }
        }
    }

    pub fn find_mut(&mut self, idx: u32) -> (u32, &mut N) {
        let (idx, _) = self.find(idx);

        (
            idx,
            match &mut self.nodes[idx as usize] {
                DisjointNode::Descendant(_) => unreachable!(),
                DisjointNode::Root(data) => data,
            },
        )
    }

    pub fn union(&mut self, left: u32, right: u32, merge: impl FnOnce(N, N) -> N) -> u32 {
        // Determine the roots of both sets
        let (left, _) = self.find(left);
        let (right, _) = self.find(right);

        // Take out the value from the left root.
        let left_data = match replace(
            &mut self.nodes[left as usize],
            DisjointNode::Descendant(Cell::new(0)),
        ) {
            DisjointNode::Descendant(_) => unreachable!(),
            DisjointNode::Root(data) => data,
        };

        // Take out the value from the right root and turn it into a descendant of the left node.
        let right_data = match replace(
            &mut self.nodes[right as usize],
            DisjointNode::Descendant(Cell::new(left)),
        ) {
            DisjointNode::Descendant(_) => unreachable!(),
            DisjointNode::Root(data) => data,
        };

        // Transform the left node back into a root with the merged value.
        self.nodes[left as usize] = DisjointNode::Root(merge(left_data, right_data));

        left
    }
}
