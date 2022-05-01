struct PQueueSlot<T> {
    priority: isize,
    data: T
}

impl<T: Clone> Clone for PQueueSlot<T> {

    fn clone(&self) -> Self {
        PQueueSlot {
            priority: self.priority,
            data: self.data.clone()
        }
    }
}

pub struct PQueueIter<'a, T> {
    iter: std::slice::Iter<'a, PQueueSlot<T>>
}

impl<'a, T: 'a> Iterator for PQueueIter<'a, T> {
    type Item = (&'a T, isize);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|v| (&v.data, v.priority))
    }
}

pub struct PriorityQueue<T: PartialEq> {
    content: Vec<PQueueSlot<T>>
}

#[derive(Debug, PartialEq)]
pub enum InsertOrDecreaseResult {
    Inserted,
    Decreased,
    Ignored
}

impl<T: Clone + PartialEq> Clone for PriorityQueue<T> {

    fn clone(&self) -> Self {
        PriorityQueue { content: self.content.clone() }
    }
}

impl<T: PartialEq> PriorityQueue<T> {

    /// Creates a new empty PriorityQueue.
    pub fn new() -> PriorityQueue<T> {
        PriorityQueue {
            content: Vec::new()
        }
    }

    /// Returns the current length of the PriorityQueue.
    pub fn len(&self) -> usize {
        self.content.len()
    }

    /// Moves the element at `index` up the heap, until
    /// the min-heap property is satisfied.
    fn move_up(&mut self, index: usize) -> () {
        if index == 0 {
            return;
        }

        let parent_index = (index - 1) / 2;
        if self.content[index].priority < self.content[parent_index].priority {
            self.content.swap(index, parent_index);

            // Scary tail-call, but this makes the function nicer and the variables immutable.
            // And if this ever busts the stack we'd already have an impossibly large heap anyways.
            // Size of heap ~= 2^(size of stack)
            self.move_up(parent_index);
        }
    }

    /// Moves the element at `index` down the heap, until
    /// the min-heap property is satisfied.
    fn move_down(&mut self, index: usize) -> () {
        let vec = &mut self.content;
        let len = vec.len();
        let left_child = index * 2 + 1;
        let right_child = index * 2 + 2;
        let smallest_child = if right_child < len && vec[right_child].priority < vec[left_child].priority {
            right_child
        } else if left_child < len {
            left_child
        } else {
            return;
        };

        if vec[smallest_child].priority < vec[index].priority {
            vec.swap(index, smallest_child);
            self.move_down(smallest_child);
        }
    }

    /// Inserts `elem` into the PriorityQueue with priority `priority`.
    /// `elem` must not already be in the queue.
    ///
    /// # Panics
    ///
    /// Panics if the element was already in the queue.
    fn insert(&mut self, data: T, priority: isize) -> () {
        let len = self.content.len();
        self.content.push(PQueueSlot {
            priority,
            data
        });

        // Moving up will also add the new element to slot_index
        self.move_up(len);
    }

    /// Returns a reference to the the element with the lowest priority,
    /// without removing it from the PriorityQueue. Returns `None` if
    /// the PriorityQueue is empty.
    pub fn peek_min(&self) -> Option<(&T, isize)> {
        match self.content.get(0) {
            Some(v) => Some((&v.data, v.priority)),
            None => None
        }
    }

    /// Returns and removes the element with the lowest priority from
    /// the PriorityQueue. Returns `None` if the queue is empty.
    pub fn extract_min(&mut self) -> Option<(T, isize)> {
        if self.content.len() == 0 {
            None
        } else {
            let out = self.content.swap_remove(0);

            if self.content.len() > 0 {
                self.move_down(0);
            }

            Some((out.data, out.priority))
        }
    }

    /// Reduces the priority for the element at `index` to `new_priority`.
    /// `new_priority` must be smaller than the current priority for the element.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds or if the new priority is
    /// greater than the current priority.
    fn decrease_key(&mut self, index: usize, new_priority: isize) -> () {
        assert!(self.content[index].priority >= new_priority);

        self.content[index].priority = new_priority;
        self.move_up(index);
    }

    /// Inserts a new element into the PriorityQueue or decreases the priority for an existing element.
    /// If `elem` is not already in the PriorityQueue, it is added with the gived `priority`.
    /// If `elem` is already in the queue, its priority is updated if `priority` is lower than
    /// its current priority, otherwise it is ignored. Whether `elem` was added, decreased or
    /// ignored is indicated by the return value.
    pub fn insert_or_decrease(&mut self, elem: T, priority: isize) -> InsertOrDecreaseResult {
        for (i, slot) in self.content.iter().enumerate() {
            if slot.data == elem {
                if slot.priority > priority {
                    self.decrease_key(i, priority);
                    return InsertOrDecreaseResult::Decreased;
                }
                return InsertOrDecreaseResult::Ignored;
            }
        }

        // Element not found, insert.
        self.insert(elem, priority);
        return InsertOrDecreaseResult::Inserted;
    }

    pub fn iter(&self) -> PQueueIter<T> {
        PQueueIter {
            iter: self.content.iter()
        }
    }
}

#[cfg(test)]
mod test {

    use super::PriorityQueue;
    use quickcheck_macros::quickcheck;

    #[quickcheck]
    fn extract_min_returns_elements_in_sorted_order(data: Vec<i32>) -> bool {
        if data.len() == 0 {
            return true;
        }

        let mut pqueue = PriorityQueue::<i32>::new();
        data.iter().for_each(|v| { let _ = pqueue.insert_or_decrease(*v, *v as isize); });
        let len = pqueue.len();

        let mut prev_min = *pqueue.peek_min().unwrap().0;
        for _ in 0..len {
            match pqueue.extract_min() {
                Some((v, _)) => if v < prev_min {
                    return false;
                } else {
                    prev_min = v;
                },
                None => {
                    return false;
                }
            }
        }
        return pqueue.extract_min() == None;
    }
}
