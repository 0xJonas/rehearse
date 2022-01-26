use std::hash::Hash;
use std::collections::HashMap;

struct PQueueSlot<T> {
    priority: isize,
    data: T
}

pub struct PriorityQueue<T: Hash + Eq + Clone> {
    content: Vec<PQueueSlot<T>>,
    slot_index: HashMap<T, usize>
}

#[derive(Debug, PartialEq)]
pub enum InsertOrDecreaseResult {
    Inserted,
    Decreased,
    Ignored
}

impl<T: Hash + Eq + Clone> PriorityQueue<T> {

    /// Creates a new empty PriorityQueue.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::priority_queue::PriorityQueue;
    /// let pqueue = PriorityQueue::<isize>::new();
    /// 
    /// assert_eq!(pqueue.len(), 0);
    /// ```
    pub fn new() -> PriorityQueue<T> {
        PriorityQueue {
            content: Vec::new(),
            slot_index: HashMap::new()
        }
    }

    /// Returns the current length of the PriorityQueue.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::priority_queue::PriorityQueue;
    /// let mut pqueue = PriorityQueue::<&str>::new();
    /// 
    /// pqueue.insert_or_decrease("a", 5);
    /// pqueue.insert_or_decrease("b", 3);
    /// pqueue.insert_or_decrease("c", 4);
    /// 
    /// assert_eq!(pqueue.len(), 3);
    /// ```
    pub fn len(&self) -> usize {
        self.content.len()
    }

    /// Moves the element at `index` up the heap, until
    /// the min-heap property is satisfied.
    fn move_up(&mut self, index: usize) -> () {
        if index == 0 {
            let new_val = &self.content[0];
            self.slot_index.insert(new_val.data.clone(), 0);
            return;
        }

        let parent_index = (index - 1) / 2;
        if self.content[index].priority < self.content[parent_index].priority {
            self.content.swap(index, parent_index);

            // Scary tail-call, but this makes the function nicer and the variables immutable.
            // And if this ever busts the stack we'd already have an impossible large heap anyways.
            // Size of heap ~= 2^(size of stack)
            self.move_up(parent_index);
        } else {
            let new_val = &self.content[index];
            self.slot_index.insert(new_val.data.clone(), index);
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
            self.slot_index.insert(self.content[index].data.clone(), index);
            return;
        };

        if vec[smallest_child].priority < vec[index].priority {
            vec.swap(index, smallest_child);
            self.move_down(smallest_child);
        } else {
            self.slot_index.insert(self.content[index].data.clone(), index);
        }
    }

    /// Inserts `elem` into the PriorityQueue with priority `priority`. 
    /// `elem` must not already be in the queue.
    /// 
    /// # Panics
    /// 
    /// Panics if the element was already in the queue.
    fn insert(&mut self, elem: T, priority: isize) -> () {
        assert!(self.slot_index.get(&elem).is_none());

        let len = self.content.len();
        self.content.push(PQueueSlot {
            priority: priority,
            data: elem
        });

        // Moving up will also add the new element to slot_index
        self.move_up(len);
    }

    /// Returns a reference to the the element with the lowest priority,
    /// without removing it from the PriorityQueue. Returns `None` if
    /// the PriorityQueue is empty.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::priority_queue::PriorityQueue;
    /// let mut pqueue = PriorityQueue::<&str>::new();
    /// 
    /// assert_eq!(pqueue.peek_min(), None);
    /// 
    /// pqueue.insert_or_decrease("a", 5);
    /// pqueue.insert_or_decrease("b", 3);
    /// pqueue.insert_or_decrease("c", 4);
    /// 
    /// assert_eq!(pqueue.peek_min(), Some(&"b"));
    /// assert_eq!(pqueue.len(), 3);
    /// ```
    pub fn peek_min(&self) -> Option<&T> {
        match self.content.get(0) {
            Some(v) => Some(&v.data),
            None => None
        }
    }

    /// Returns and removes the element with the lowest priority from
    /// the PriorityQueue. Returns `None` if the queue is empty.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::priority_queue::PriorityQueue;
    /// let mut pqueue = PriorityQueue::<&str>::new();
    /// 
    /// assert_eq!(pqueue.extract_min(), None);
    /// 
    /// pqueue.insert_or_decrease("a", 5);
    /// pqueue.insert_or_decrease("b", 3);
    /// pqueue.insert_or_decrease("c", 4);
    /// 
    /// assert_eq!(pqueue.extract_min(), Some("b"));
    /// assert_eq!(pqueue.len(), 2);
    /// ```
    pub fn extract_min(&mut self) -> Option<T> {
        if self.content.len() == 0 {
            None
        } else {
            let out = self.content.swap_remove(0).data;
            let _ = self.slot_index.remove(&out);

            if self.content.len() > 0 {
                self.move_down(0);
            }

            Some(out)
        }
    }

    /// Returns the index of `val` into the PriorityQueue,
    /// or `None` if `val` is not in the queue.
    fn index(&self, val: &T) -> Option<usize> {
        match self.slot_index.get(val) {
            Some(&v) => Some(v),
            None => None
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
        assert!(index < self.len());
        assert!(self.content[index].priority >= new_priority);

        self.content[index].priority = new_priority;
        self.move_up(index);
    }

    /// Inserts a new element into the PriorityQueue or decreases the priority for an existing element.
    /// If `elem` is not already in the PriorityQueue, it is added with the gived `priority`.
    /// If `elem` is already in the queue, its priority is updated if `priority` is lower than
    /// its current priority, otherwise it is ignored. Whether `elem` was added, decreased or
    /// ignored is indicated by the return value.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::priority_queue::{PriorityQueue, InsertOrDecreaseResult};
    /// let mut pqueue = PriorityQueue::<&str>::new();
    /// 
    /// assert_eq!(pqueue.insert_or_decrease("a", 5), InsertOrDecreaseResult::Inserted);
    /// assert_eq!(pqueue.insert_or_decrease("a", 6), InsertOrDecreaseResult::Ignored);
    /// assert_eq!(pqueue.insert_or_decrease("a", 4), InsertOrDecreaseResult::Decreased);
    /// ```
    pub fn insert_or_decrease(&mut self, elem: T, priority: isize) -> InsertOrDecreaseResult {
        match self.index(&elem) {
            Some(index) => if self.content[index].priority > priority {
                self.decrease_key(index, priority);
                return InsertOrDecreaseResult::Decreased;
            } else {
                return InsertOrDecreaseResult::Ignored;
            },
            None => {
                self.insert(elem, priority);
                return InsertOrDecreaseResult::Inserted;
            }
        }
    }
}

#[cfg(test)]
mod test {

    use crate::priority_queue::PriorityQueue;
    use quickcheck_macros::quickcheck;

    #[quickcheck]
    fn extract_min_returns_elements_in_sorted_order(data: Vec<i32>) -> bool {
        if data.len() == 0 {
            return true;
        }

        let mut pqueue = PriorityQueue::<i32>::new();
        data.iter().for_each(|v| { let _ = pqueue.insert_or_decrease(*v, *v as isize); });
        let len = pqueue.len();

        let mut prev_min = *pqueue.peek_min().unwrap();
        for _ in 0..len {
            match pqueue.extract_min() {
                Some(v) => if v < prev_min {
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