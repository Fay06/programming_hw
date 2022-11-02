/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0{
        -1
    }else{
        let mut sum = 0;
        for i in 1..=n{
            sum += i;
        }
        sum
    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut sum = 0;
    for i in ls.iter(){
        if i >= &s && i <= &e{
            sum += 1;
        }
    }
    sum
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for i in target.iter(){
        if !set.contains(&i){
            return false;
        }
    }
    true
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    let mut sum = 0.0;
    for i in ls.iter(){
        sum += i;
    }
    if ls.len() == 0{
        None
    }else{
        Some(sum/(ls.len() as f64))
    } 
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    let mut exp = ls.len();
    let mut ans = 0;
    for i in ls.iter(){
        exp -= 1;
        ans += i * 2_i32.pow(exp as u32); 
    }
    ans
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut arr = Vec::new();
    let mut num = n;
    while num != 1{
        for i in 2..=num{
            if num % i == 0{
                arr.push(i);
                num = num/i;
            }
        }
    }
    arr
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut arr = Vec::new();
    if lst.len() == 0{
        arr
    }else{
        for &i in lst.iter(){
            arr.push(i);
        }
        arr.remove(0);
        arr.push(lst[0]);
        arr
    }  
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    if s.contains(target){
        true
    }else{
        false
    }
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s == ""{
        None
    }else{
        let mut count = 0;
        let mut ans_index = 0;
        let mut curr = 0;
        let mut curr_index = 0;
        for i in 0..s.len(){
            let curr_char = s.chars().nth(i).unwrap();
            curr_index = i;
            curr = 1;
            for j in (i + 1)..s.len(){
                if s.chars().nth(j).unwrap() == curr_char{
                    curr += 1;
                }else{
                    break;
                }
            }
            
            if count < curr{
                ans_index = curr_index;
                count = curr;
            }
        }
        s.get(ans_index..(ans_index + count))
    }   
}
