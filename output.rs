fn printl<T: std::fmt::Display>(val: T) -> T {
    println!("{}", val);
    val
}

    pub fn sum(a: i32, b: i32) -> i32 {
    return a + &(b);
}

#[derive(Debug, Clone)]
struct Address {
    pub street: String,
    pub number: i32,
    pub city: String,
    pub state: String,
    pub zip_code: String,
}

#[derive(Debug, Clone)]
struct Person {
    pub name: String,
    pub age: i32,
    pub is_adult: bool,
    pub height: f64,
    pub address: Address,
    pub friends: Vec<Person>,
}

#[derive(Debug, Clone, PartialEq)]
enum Animal_type {
    cat,
    dog,
    bird,
    snake,
}

#[derive(Debug, Clone)]
struct Animal {
    pub name: String,
    pub r#type: Animal_type,
    pub is_dangerous: bool,
}

impl Animal {
    pub fn new(name: String, r#type: Animal_type) -> Self {
        let mut this_obj = Self {
            name: String::new(),
            r#type: Animal_type::cat,
            is_dangerous: false
        };
        let mut self_ = Animal {
            name: String::new(),
            r#type: Animal_type::cat,
            is_dangerous: false
        };
        let mut this_obj = self_;
    this_obj.name = name;
    this_obj.r#type = r#type;
        this_obj

    }
    pub fn make_sound(&mut self) {
        let this_obj = self;
    if this_obj.r#type == (Animal_type::cat) {
    printl("Meow".to_string());
    } else {
    if this_obj.r#type == (Animal_type::dog) {
    printl("Woof".to_string());
    } else {
    if this_obj.r#type == (Animal_type::bird) {
    printl("Chirp".to_string());
    } else {
    if this_obj.r#type == (Animal_type::snake) {
    printl("Hiss".to_string());
    }
    }
    }
    }
}
    fn set_is_dangerous(&mut self) {
        let this_obj = self;
    if this_obj.r#type == (Animal_type::snake) {
    this_obj.is_dangerous = true;
    } else {
    this_obj.is_dangerous = false;
    }
}
}

    pub fn user_main() {
    let x: i32;
    let name: String = "Matheus".to_string();
    x = 1;
    let y: i32 = sum(x, 2);
    let z: i32 = printl(sum(x, 3));
    let mut num: i32 = 1;
    num = 2;
    printl(num);
    if x > (2) {
    printl("x is greater than 2".to_string());
    } else {
    printl("x is not greater than 2".to_string());
    }
    let mut r#loop: i32 = 1;
    loop {
    printl(r#loop);
    r#loop = r#loop + &(1);
        if !(r#loop < (10)) { break; }
    }
    printl(x);
    printl("Hello ".to_string() + &(name));
}

fn main() {
    let matheus: Person = Person { name: "Matheus".to_string(), age: 20, is_adult: true, height: 1.78, address: Address { street: "Rua 1".to_string(), number: 1, city: "São Paulo".to_string(), state: "SP".to_string(), zip_code: "00000-000".to_string() }, friends: vec![] };
    let julia: Person = Person { name: "Julia".to_string(), age: 21, is_adult: true, height: 1.65, address: Address { street: "Rua 2".to_string(), number: 2, city: "São Paulo".to_string(), state: "SP".to_string(), zip_code: "00000-000".to_string() }, friends: vec![matheus] };
    matheus.friends.push(julia);
    printl(matheus.friends[0 as usize].name);
    let cat: Animal = Animal::new("Jasmini".to_string(), Animal_type::cat);
    cat.make_sound();
    cat.set_is_dangerous();
    printl(cat.name + &(" is dangerous: ".to_string()) + &(cat.is_dangerous));
    user_main();
}
