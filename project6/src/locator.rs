use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/** 
    These traits are implemented for Nodes to make them comparable 
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/** 
    You must implement the above trait for the vector type 
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
    /**
        This functions pushes a given element onto the queue and
        reorders the queue such that the min heap property holds.
        See the project specifications for more details on how this
        works.
    **/
    fn enqueue(&mut self, ele: T) -> () {
        let mut child = self.len();
        let mut parent = if child <= 1 {0} else {(child - 1) / 2};
        self.push(ele);

        while parent >= 0 {
            if self[parent] > self[child] {
                self.swap(parent, child);
                child = parent;
                parent = if child <= 1 {0} else {(child - 1) / 2};
            } else {
                break;
            }     
        }
    }

    /**
        This function removes the root element from the queue and
        reorders the queue such that it maintains the min heap
        property.  See the project specifications for more details.
        You should return the deleted element in the form of an option.
        Return None if the queue was initially empty, Some(T) otherwise.
    **/
    fn dequeue(&mut self) -> Option<T> {
        if self.len() == 0 {
            None
        } else if self.len() == 1{
            let del = self.remove(0);
            Some(del)
        } else {
            let del = self.remove(0);
            let last = self.pop().unwrap();
            self.insert(0, last);
            let mut parent = 0;
            let mut child = (parent * 2) + 1;
            while child < self.len() {
                if child < self.len() - 1 && self[child + 1] < self[child] {
                    if self[child + 1] < self[parent] {
                        self.swap(parent, child + 1);
                        parent = child + 1;
                        child = (parent * 2) + 1;
                    } else {
                        break;
                    }
                }else {
                    if self[child] < self[parent] {
                        self.swap(parent, child);
                        parent = child;
                        child = (parent * 2) + 1;
                    } else {
                        break;
                    }
                }
            }
            Some(del)
        }
    }

    /**
        This function returns the element that would be removed
        if dequeue were called on the queue.  There should be no
        mutations to the queue.  Return the element in the form
        of an option.  Return None if the queue is empty, Some(T)
        otherwise.
    **/
    fn peek(&self) -> Option<&T> {
        if self.len() == 0 {
            None
        } else {
            Some(self.get(0).unwrap())
        }
    }
}


/**
    You must implement this function that computes the orthogonal
    distance between two coordinates.  Remember, orthogonal distance
    is not like Euclidean distance.  See the specifications for more
    details.
**/
pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let (x1, y1) = p1;
    let (x2, y2) = p2;
    (x1 - x2).abs() + (y1 - y2).abs()
}

/**
    You must implement this function that determines which enemy Stark
    should battle and their coordinates.  You are given two hashmaps for
    allies and enemies.  Each maps a name to their current coordinates.
    You can assume that the allies hashmap will always have a name
    called "Stark" included.  Return the name and coordinates of the enemy
    Stark will battle in the form of a 3-tuple.  See the specifications
    for more details on how to choose which enemy.
**/
pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
    let mut queue = Vec::new();   
    for (a_key, a_value) in allies.iter() {
        for (e_key, e_value) in enemies.iter() {
            let node = Node {priority: distance(*a_value, *e_value), data: (a_key, e_key)};
            queue.enqueue(node);
        }
    }
    let mut attacked = Vec::new();
    while queue.peek() != None {
        let curr = queue.dequeue().unwrap();
        let (a, e) = curr.data;
        if a.to_string() == "Stark" && !attacked.contains(e){
            let (e1, e2) = enemies.get(e).unwrap();
            return (e, *e1, *e2);
        }
        attacked.push(e);
    }
    ("", -1, -1)
}


