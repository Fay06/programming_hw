#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        let mut ans = String::new();
        match self{
            |Command::Power(bol,num) => {
                                            ans.push_str("Power ");
                                            if *bol {ans.push_str("increased ");} else {ans.push_str("decreased ");}
                                            ans.push_str("by ");
                                            ans.push_str(&num.to_string());
                                            ans.push_str("%");
                                            ans
                                        }
            |Command::Missiles(bol,num) => {
                                            ans.push_str("Missiles ");
                                            if *bol {ans.push_str("increased ");} else {ans.push_str("decreased ");}
                                            ans.push_str("by ");
                                            ans.push_str(&num.to_string());
                                            ans
                                        }
            |Command::Shield(bol) => {
                                        ans.push_str("Shield turned ");
                                        if *bol {ans.push_str("on");} else {ans.push_str("off");}
                                        ans
                                    }  
            |Command::Try => {
                                ans.push_str("Call attempt failed");
                                ans
                            } 
            |Command::Invalid => {
                                ans.push_str("Not a command");
                                ans
                            } 
        }
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    let v: Vec<&str> = s.split(' ').collect();
    if v.len() == 3 && v[0] == "power" {
        if v[1] == "inc" {
            Command::Power(true, v[2].parse().unwrap())
        } else if v[1] == "dec" {
            Command::Power(false, v[2].parse().unwrap())
        } else {
            Command::Invalid
        }
    }else if v.len() == 3 && v[2] == "missiles"{
        if v[0] == "add" {
            Command::Missiles(true, v[1].parse().unwrap())
        } else if v[0] == "fire" {
            Command::Missiles(false, v[1].parse().unwrap())
        } else {
            Command::Invalid
        }
    } else if v.len() == 2 && v[0] == "shield" {
        if v[1] == "on" {
            Command::Shield(true)
        } else if v[1] == "off" {
            Command::Shield(false)
        } else {
            Command::Invalid
        }
    } else if s == "try calling Miss Potts" {
        Command::Try
    } else {
        Command::Invalid
    }
}
